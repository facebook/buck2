/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#include <folly/DynamicConverter.h>
#include <folly/Subprocess.h>
#include <folly/coro/BlockingWait.h>
#include <folly/coro/Collect.h>
#include <folly/coro/SharedMutex.h>
#include <folly/coro/Task.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/executors/IOThreadPoolExecutor.h>
#include <folly/init/Init.h>
#include <folly/json.h>
#include <string.h>
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <vector>
#include "common/files/FileUtil.h"

namespace {

static std::string metaFileInputPath;
static std::string indexFileOutputPath;
static std::string linkPlanFileOutputPath;
static std::string finalIndexFileOutputPath;
static bool enablePreMerger = false;
static std::vector<std::string> linkerInvocationCommand;
folly::coro::SharedMutex absorbedBitcodeFilesMutex;
folly::coro::SharedMutex nonLTOObjectFilesMutex;

struct ObjectFileRecord {
  const std::string inputObjectFilePath;
  const int starlarkArrayIndex;
  const std::string outputPlanFilePath;
  ObjectFileRecord(
      const std::string& inputObjectFilePath,
      const int starlarkArrayIndex,
      const std::string& outputPlanFilePath)
      : inputObjectFilePath(inputObjectFilePath),
        starlarkArrayIndex(starlarkArrayIndex),
        outputPlanFilePath(outputPlanFilePath) {}
  virtual ~ObjectFileRecord() = default;
};

struct StandaloneObjectFile : ObjectFileRecord {
  const std::string outputIndexShardFilePath;
  const std::optional<std::string> outputPremergedBitcodeFilePath;

  StandaloneObjectFile(
      const std::string& inputObjectFilePath,
      const std::string& outputIndexShardFilePath,
      const std::string& outputPremergedBitcodeFilePath,
      const std::string& outputPlanFilePath,
      const int starlarkArrayIndex)
      : ObjectFileRecord(
            inputObjectFilePath,
            starlarkArrayIndex,
            outputPlanFilePath),
        outputIndexShardFilePath(outputIndexShardFilePath),
        outputPremergedBitcodeFilePath(outputPremergedBitcodeFilePath) {}
};

struct ExtractedObjectFile : ObjectFileRecord {
  const std::string outputIndexShardsDirectoryPath;
  const std::optional<std::string> outputPreMergedBitcodeDirectoryPath;

  ExtractedObjectFile(
      const std::string& inputObjectFilePath,
      const std::string& outputIndexShardsDirectoryPath,
      const std::string& outputPremergedBitcodeDirectoryPath,
      const std::string& outputPlanFilePath,
      const int starlarkArrayIndex)
      : ObjectFileRecord(
            inputObjectFilePath,
            starlarkArrayIndex,
            outputPlanFilePath),
        outputIndexShardsDirectoryPath(outputIndexShardsDirectoryPath),
        outputPreMergedBitcodeDirectoryPath(
            outputPremergedBitcodeDirectoryPath) {}
};

struct Archive {
  std::vector<std::shared_ptr<const ExtractedObjectFile>> objects;
  const std::string outputIndexShardsDirectoryPath;
  const std::optional<std::string> outputPremergedBitcodeDirectoryPath;
  const std::string outputPlanFilePath;
};

enum class BitcodeMergeState {
  Standalone,
  Absorbed,
  Root,
  NotLoaded,
};

struct ExtractedObjectFileOptimizationPlan {
  std::vector<int> objectFileImports;
  std::vector<int> archiveImports;
  bool isBitcode;
  std::string path;
  std::optional<std::string> indexShardFilePath;
  bool loadedByLinker;
  std::optional<BitcodeMergeState> bitcodeMergeState;
  std::optional<std::string> mergedBitcodePath;
};

static const folly::json::serialization_opts jsonSerializationOptions = []() {
  folly::json::serialization_opts sorted;
  sorted.sort_keys = true;
  return sorted;
}();

static void parseArgs(int argc, char** argv) {
  int position = 1;
  const auto boundsCheckFlag = [&](const std::string& flag) {
    if (position + 1 >= argc) {
      std::cerr << "Missing argument for flag: " << flag << std::endl;
      std::abort();
    }
  };

  while (position < argc) {
    if (strcmp(argv[position], "--meta") == 0) {
      boundsCheckFlag("--meta");
      metaFileInputPath = argv[position + 1];
      position += 2;
    } else if (strcmp(argv[position], "--index") == 0) {
      boundsCheckFlag("--index");
      indexFileOutputPath = argv[position + 1];
      position += 2;
    } else if (strcmp(argv[position], "--link-plan") == 0) {
      boundsCheckFlag("--link-plan");
      linkPlanFileOutputPath = argv[position + 1];
      position += 2;
    } else if (strcmp(argv[position], "--final-link-index") == 0) {
      boundsCheckFlag("--final-link-index");
      finalIndexFileOutputPath = argv[position + 1];
      position += 2;
    } else if (strcmp(argv[position], "--enable-premerger") == 0) {
      enablePreMerger = true;
      position += 1;
    } else if (strcmp(argv[position], "--") == 0) {
      for (position++; position < argc; position++) {
        linkerInvocationCommand.emplace_back(argv[position]);
      }
    } else {
      std::cerr << "Could not recognize argument: " << argv[position]
                << std::endl;
      std::abort();
    }
  }
}

enum class MetaFileRecordType {
  ArchiveMember,
  ObjectFile,
};

constexpr std::string_view EXTRACTED_OBJECT_FILE_RECORD_TYPE_SPELLING =
    "ARCHIVE_MEMBER";
constexpr std::string_view OBJECT_FILE_RECORD_TYPE_SPELLING = "OBJECT_FILE";
constexpr std::string_view MERGED_BITCODE_SUFFIX = ".merged.bc";
constexpr std::string_view SUMMARY_SHARD_SUFFIX = ".thinlto.bc";
constexpr std::string_view IMPORTS_FILE_SUFFIX = ".imports";
constexpr std::string_view OPTIMIZED_BITCODE_SUFFIX = ".opt.o";

using ObjectFileRecordsMapTy =
    std::unordered_map<std::string, std::shared_ptr<const ObjectFileRecord>>;

static void parseMetaFileRecords(
    ObjectFileRecordsMapTy& objectFileRecordsMap,
    std::unordered_map<int, std::unique_ptr<Archive>>& archiveMap) {
  std::ifstream metaFile(metaFileInputPath);
  if (!metaFile.is_open()) {
    std::cerr << "Failed to open file: " << metaFileInputPath << std::endl;
    std::abort();
  }
  std::stringstream buffer;
  buffer << metaFile.rdbuf();
  try {
    auto jsonData = folly::parseJson(buffer.str());
    for (const auto& record : jsonData) {
      if (record["record_type"].getString() ==
          EXTRACTED_OBJECT_FILE_RECORD_TYPE_SPELLING) {
        const std::string emptyString;
        const std::string outputPremergedBitcodeDirectoryPath = enablePreMerger
            ? record["output_premerged_bitcode_directory_path"].getString()
            : emptyString;
        std::shared_ptr<const ExtractedObjectFile>
            parsedExtractedObjectFileRecord =
                std::make_shared<ExtractedObjectFile>(
                    record["input_object_file_path"].getString(),
                    record["output_index_shards_directory_path"].getString(),
                    outputPremergedBitcodeDirectoryPath,
                    record["output_plan_file_path"].getString(),
                    record["starlark_array_index"].getInt());

        if (!archiveMap.contains(
                parsedExtractedObjectFileRecord->starlarkArrayIndex)) {
          archiveMap[parsedExtractedObjectFileRecord->starlarkArrayIndex] =
              std::make_unique<Archive>(
                  std::vector<std::shared_ptr<const ExtractedObjectFile>>(),
                  parsedExtractedObjectFileRecord
                      ->outputIndexShardsDirectoryPath,
                  parsedExtractedObjectFileRecord
                      ->outputPreMergedBitcodeDirectoryPath,
                  parsedExtractedObjectFileRecord->outputPlanFilePath);
        }

        archiveMap[parsedExtractedObjectFileRecord->starlarkArrayIndex]
            ->objects.push_back(parsedExtractedObjectFileRecord);

        objectFileRecordsMap[parsedExtractedObjectFileRecord
                                 ->inputObjectFilePath] =
            parsedExtractedObjectFileRecord;
      } else if (
          record["record_type"].getString() ==
          OBJECT_FILE_RECORD_TYPE_SPELLING) {
        const std::string emptyString;
        const std::string outputPremergedBitcodeFilePath = enablePreMerger
            ? record["output_premerged_bitcode_file_path"].getString()
            : emptyString;
        std::shared_ptr<const StandaloneObjectFile> parsedObjectFileRecord =
            std::make_shared<StandaloneObjectFile>(
                record["input_object_file_path"].getString(),
                record["output_index_shard_file_path"].getString(),
                outputPremergedBitcodeFilePath,
                record["output_plan_file_path"].getString(),
                record["starlark_array_index"].getInt());
        objectFileRecordsMap[parsedObjectFileRecord->inputObjectFilePath] =
            parsedObjectFileRecord;
      } else {
        std::cerr << "Unknown record type: " << record["type"].getString()
                  << std::endl;
        std::abort();
      }
    }
  } catch (const folly::json::parse_error& e) {
    std::cerr << "Error parsing JSON: " << e.what() << std::endl;
    std::abort();
  }
}

static void populatePreMergerPathConversionMaps(
    const ObjectFileRecordsMapTy& objectFileRecordsMap,
    std::unordered_map<std::string, std::string>& preMergeToPostMerge,
    std::unordered_map<std::string, std::string>& postMergeToPreMerge) {
  for (const auto& [preMergePath, objectFileRecord] : objectFileRecordsMap) {
    std::string postMergePath;
    if (const auto* objectFile =
            dynamic_cast<const StandaloneObjectFile*>(objectFileRecord.get())) {
      postMergePath = objectFile->outputPremergedBitcodeFilePath.value();
    } else if (
        const auto* extractedObjectFile =
            dynamic_cast<const ExtractedObjectFile*>(objectFileRecord.get())) {
      postMergePath +=
          extractedObjectFile->outputPreMergedBitcodeDirectoryPath.value();
      postMergePath += "/";
      postMergePath += preMergePath;
      postMergePath += MERGED_BITCODE_SUFFIX;
    } else {
      std::cerr << "Unrecognized object file record" << std::endl;
      std::abort();
    }

    preMergeToPostMerge[preMergePath] = postMergePath;
    postMergeToPreMerge[postMergePath] = preMergePath;
  }
}

static void runThinLink(
    const std::unordered_map<std::string, std::string>& preMergeToPostMerge) {
  if (enablePreMerger) {
    // Buck requires actions write output files to a location set by the build
    // system.  When toolchain binaries cannot for whatever reason easily
    // produce output files at known locations configurable locations, the usual
    // approach is to use a wrapper Python script such as this one to place the
    // files at the locations buck expects.  In this case, this will not work as
    // sharded summaries embed paths to other bitcode files within the bitcode.
    // That means the toolchain must write the merged bitcode files to locations
    // buck expects in the first place. This json document is parsed by the
    // linker and communicates the locations at which the toolchain must write
    // the merged bitcode files.
    facebook::files::TemporaryFile preMergerOutputFileMappingFile(
        nullptr, "premergerOutputMap", /* keep */ true);
    std::ofstream stream(preMergerOutputFileMappingFile.filename());
    stream << folly::toJson(folly::toDynamic(preMergeToPostMerge));
    stream.flush();
    std::string preMergerMapFlag = "-Wl,-mllvm,-premerger-output-map=";
    preMergerMapFlag += preMergerOutputFileMappingFile.filename();
    linkerInvocationCommand.push_back((preMergerMapFlag));
  }

  folly::Subprocess subprocess(
      linkerInvocationCommand, folly::Subprocess::Options().usePath());
  subprocess.waitChecked();
}

static std::filesystem::path thinLTOPrefixReplacedPath(
    const std::filesystem::path& path) {
  // The linker is passed --thinlto-prefix-replace=";<indexFileOutPath>" which
  // instructs the linker to prefix various paths with the value of
  // <indexFileOutPath>.
  return std::filesystem::path(indexFileOutputPath) / path;
}

static std::unordered_set<std::string> parseLoadedInputBitcodeFiles() {
  // Thin-link will write two "index" files, one named "index" the other
  // "index.full". We read the first to get the set of bitcode files that were
  // loaded. Notably this will not include native object files fed to the link
  // directly. This set will be used to avoid creating opt action for bitcode we
  // don't load anyways.
  std::unordered_set<std::string> loadedInputBitcodeFiles;
  std::ifstream stream(thinLTOPrefixReplacedPath("index"));
  std::string line;
  while (std::getline(stream, line)) {
    std::string normalizedBitcodeFilePath =
        line.substr(indexFileOutputPath.size() + 1);
    loadedInputBitcodeFiles.insert(normalizedBitcodeFilePath);
  }
  return loadedInputBitcodeFiles;
}

static const std::vector<std::string> readImportsFile(
    const std::string& importsFilePath,
    const std::unordered_map<std::string, std::string>& postMergeToPreMerge) {
  std::ifstream stream(importsFilePath);
  std::string line;
  std::vector<std::string> imports;
  while (std::getline(stream, line)) {
    if (enablePreMerger) {
      if (line.ends_with(MERGED_BITCODE_SUFFIX)) {
        imports.push_back(postMergeToPreMerge.at(line));
      } else {
        imports.push_back(line);
      }
    } else {
      imports.push_back(line);
    }
  }
  return imports;
}

static std::string getBitcodeMergeStateSpelling(const BitcodeMergeState state) {
  switch (state) {
    case BitcodeMergeState::Standalone:
      return "STANDALONE";
    case BitcodeMergeState::Absorbed:
      return "ABSORBED";
    case BitcodeMergeState::Root:
      return "ROOT";
    case BitcodeMergeState::NotLoaded:
      return "NOT_LOADED";
  }
}

static bool isFileLLVMBitcodeWrapperFile(const std::string& filePath) {
  const uint32_t LLVM_BITCODE_WRAPPER_FILE_MAGIC = 0xB17C0DE;
  std::ifstream stream(filePath, std::ios::binary);
  uint32_t magic;
  char buffer[sizeof(magic)];
  if (stream.read(buffer, sizeof(magic))) {
    memcpy(&magic, buffer, sizeof(magic));
    return magic == LLVM_BITCODE_WRAPPER_FILE_MAGIC;
  } else {
    std::cerr << "Failed to read magic number from file: " << filePath
              << std::endl;
    std::abort();
  }
}

static BitcodeMergeState readMergedBitcodeFile(
    const std::string& mergedBitcodePath) {
  if (!std::filesystem::file_size(mergedBitcodePath)) {
    return BitcodeMergeState::NotLoaded;
  }

  if (isFileLLVMBitcodeWrapperFile(mergedBitcodePath)) {
    return BitcodeMergeState::Root;
  }

  std::ifstream stream(mergedBitcodePath);
  std::string line;
  while (std::getline(stream, line)) {
    if (line == "standalone") {
      return BitcodeMergeState::Standalone;
    }
    if (line == "absorbed") {
      return BitcodeMergeState::Absorbed;
    }
  }

  std::cerr << "Ill-formatted merged bitcode file: " << mergedBitcodePath
            << std::endl;
  std::abort();
}

static void writeStandaloneObjectFileOptimizationPlanToJSONFile(
    const std::vector<int>& objectFileImports,
    const std::vector<int>& archiveImports,
    const bool isBitcode,
    const std::optional<BitcodeMergeState> bitcodeMergeState,
    const bool loadedByLinker,
    const std::string& outputPath) {
  std::ofstream stream(outputPath);
  folly::dynamic json = folly::dynamic::object();
  json["imports"] = folly::toDynamic(objectFileImports);
  json["archive_imports"] = folly::toDynamic(archiveImports);
  json["is_bitcode"] = isBitcode;
  if (bitcodeMergeState) {
    json["merge_state"] =
        getBitcodeMergeStateSpelling(bitcodeMergeState.value());
  }
  json["loaded_by_linker"] = loadedByLinker;
  stream << folly::json::serialize(json, jsonSerializationOptions);
}

static void writeArchiveOptimizationPlanTOJSONFile(
    const std::vector<ExtractedObjectFileOptimizationPlan>& objectPlans,
    const std::string& baseDir,
    const std::string& outputPath) {
  std::ofstream stream(outputPath);
  folly::dynamic json = folly::dynamic::object();
  folly::dynamic objectPlansJsonArray = folly::dynamic::array();
  for (const auto& extractedObjectFileOptimizationPlan : objectPlans) {
    folly::dynamic objectPlanJson = folly::dynamic::object();
    objectPlanJson["imports"] =
        folly::toDynamic(extractedObjectFileOptimizationPlan.objectFileImports);
    objectPlanJson["archive_imports"] =
        folly::toDynamic(extractedObjectFileOptimizationPlan.archiveImports);
    objectPlanJson["is_bitcode"] =
        extractedObjectFileOptimizationPlan.isBitcode;
    objectPlanJson["path"] = extractedObjectFileOptimizationPlan.path;
    objectPlanJson["loaded_by_linker"] =
        extractedObjectFileOptimizationPlan.loadedByLinker;
    if (extractedObjectFileOptimizationPlan.indexShardFilePath) {
      objectPlanJson["index_shard_file_path"] =
          extractedObjectFileOptimizationPlan.indexShardFilePath.value();
    }
    if (extractedObjectFileOptimizationPlan.bitcodeMergeState) {
      objectPlanJson["bitcode_merge_state"] = getBitcodeMergeStateSpelling(
          extractedObjectFileOptimizationPlan.bitcodeMergeState.value());
    }
    if (extractedObjectFileOptimizationPlan.mergedBitcodePath) {
      objectPlanJson["merged_bitcode_path"] =
          extractedObjectFileOptimizationPlan.mergedBitcodePath.value();
    }
    objectPlansJsonArray.push_back(objectPlanJson);
  }
  json["object_plans"] = objectPlansJsonArray;
  json["base_dir"] = baseDir;
  stream << folly::json::serialize(json, jsonSerializationOptions);
}

static void writeLinkPlan(std::vector<int>& nonLTOObjectFiles) {
  std::ofstream stream(linkPlanFileOutputPath);
  folly::dynamic json = folly::dynamic::object;
  folly::dynamic nonLTOObjectsJson = folly::dynamic::object();
  for (const auto starlarkIndex : nonLTOObjectFiles) {
    nonLTOObjectsJson[std::to_string(starlarkIndex)] = 1;
  }
  json["non_lto_objects"] = nonLTOObjectsJson;
  stream << folly::json::serialize(json, jsonSerializationOptions);
}

// The "index.full" file is a filelist that will be used as input to the
// filelink, providing a list and order in which to provide the native object
// files to the final link. However, it refers to input bitcode files by their
// name as provided to thin-link (potentially with a prefix as specified by
// thinlto-prefix-replace). Opt + codegen actions will consume these bitcode
// files and write them elsewhere. This step takes this filelist and translates
// input bitcode file paths to the final path where the native object files will
// be written.
static void writeFinalLinkIndex(
    const std::unordered_set<std::string>& absorbedBitcodeFiles,
    const ObjectFileRecordsMapTy& objectFileRecordsMap) {
  std::ofstream writeStream(finalIndexFileOutputPath);
  std::ifstream readStream(thinLTOPrefixReplacedPath("index.full"));
  std::string line;

  auto replace = [](std::string& target,
                    const std::string_view toReplace,
                    const std::string_view replaceWith) {
    auto pos = target.find(toReplace);
    if (pos == std::string::npos) {
      return;
    }

    target.replace(pos, toReplace.size(), replaceWith);
  };

  // This file will contain the set of loaded files, both bitcode and native
  // object files. Bitcode files will be prefixed with the directory where we
  // write the index file. This is because we pass
  // thinlto-prefix-replace=";<index_out_dir>" to the linker.
  while (std::getline(readStream, line)) {
    // By checking if the file is prefixed with the index output path, we are
    // checking if it is bitcode.
    if (line.size() >= indexFileOutputPath.size() &&
        line.substr(0, indexFileOutputPath.size()) == indexFileOutputPath) {
      std::string normalizedPath = line.substr(indexFileOutputPath.size() + 1);

      // The premerger will merge some bitcode files into others, and we need to
      // remove the paths referring to absorbed files.
      if (absorbedBitcodeFiles.contains(normalizedPath)) {
        continue;
      }

      if (const auto* standaloneObjectFile =
              dynamic_cast<const StandaloneObjectFile*>(
                  objectFileRecordsMap.at(normalizedPath).get())) {
        // For each standalone object file passed to the linker, in bzl logic
        // we declare a few outputs side by side (.thinlto.bc index shard,
        // .opt.plan optimization plan, .opt.o optimized bitcode file). We
        // will eventually take the input bitcode .o file, and write a native
        // object file to the declared .opt.o output location. We need to
        // convert between the two paths. We start with the output inded shard
        // file path, because the two are the same file with different
        // extensions, then we remove the .thinlto.bc and add the .opt.o. We
        // could include the optimized bitcode output location in the metafile
        // instead.
        std::string outputPath = standaloneObjectFile->outputIndexShardFilePath;
        replace(outputPath, SUMMARY_SHARD_SUFFIX, OPTIMIZED_BITCODE_SUFFIX);
        writeStream << outputPath << "\n";
      } else {
        // For object files extracted from archives via dynamic output, we
        // have a declare two output directories, one for extracted bitcode
        // files, and another for their native counterparts. We translate
        // between the two directories here.
        std::string outputPath = normalizedPath;
        replace(outputPath, "/objects/", "/opt_objects/objects/");
        writeStream << outputPath << "\n";
      }
    } else {
      // Non bitcode files should be included verbatim.
      writeStream << line << "\n";
    }
  }
}

static folly::coro::Task<void> writeOptimizationPlanForStandaloneObjectFile(
    const std::string& inputBitcodeFilePath,
    const StandaloneObjectFile* standaloneObjectFile,
    const ObjectFileRecordsMapTy& objectFileRecordsMap,
    const std::unordered_map<std::string, std::string>& postMergeToPreMerge,
    const std::unordered_set<std::string>& loadedInputBitcodeFiles,
    std::unordered_set<std::string>& absorbedBitcodeFiles,
    std::vector<int>& nonLTOObjectFiles) {
  co_await folly::coro::co_reschedule_on_current_executor;

  std::filesystem::create_directories(
      std::filesystem::path(standaloneObjectFile->outputIndexShardFilePath)
          .parent_path());

  const std::string temporaryIndexShardFilePath =
      thinLTOPrefixReplacedPath(standaloneObjectFile->inputObjectFilePath)
          .concat(SUMMARY_SHARD_SUFFIX);

  const std::string importsFilePath =
      thinLTOPrefixReplacedPath(standaloneObjectFile->inputObjectFilePath)
          .concat(IMPORTS_FILE_SUFFIX);

  if (std::filesystem::exists(importsFilePath)) {
    std::filesystem::rename(
        temporaryIndexShardFilePath,
        standaloneObjectFile->outputIndexShardFilePath);

    const auto imports = readImportsFile(importsFilePath, postMergeToPreMerge);

    std::vector<int> objectFileImports;
    std::vector<int> archiveImports;
    for (const auto& importPath : imports) {
      const auto& objectFileRecord = objectFileRecordsMap.at(importPath);
      if (const auto* importedStandaloneObjectFile =
              dynamic_cast<const StandaloneObjectFile*>(
                  objectFileRecord.get())) {
        objectFileImports.push_back(
            importedStandaloneObjectFile->starlarkArrayIndex);
      } else if (
          const auto* extractedObjectFile =
              dynamic_cast<const ExtractedObjectFile*>(
                  objectFileRecord.get())) {
        archiveImports.push_back(extractedObjectFile->starlarkArrayIndex);
      }
    }

    std::optional<BitcodeMergeState> bitcodeMergeState;
    if (enablePreMerger) {
      bitcodeMergeState = readMergedBitcodeFile(
          standaloneObjectFile->outputPremergedBitcodeFilePath.value());
      if (bitcodeMergeState == BitcodeMergeState::Absorbed) {
        co_await absorbedBitcodeFilesMutex.co_lock();
        absorbedBitcodeFiles.insert(standaloneObjectFile->inputObjectFilePath);
        absorbedBitcodeFilesMutex.unlock();
      }
    }

    writeStandaloneObjectFileOptimizationPlanToJSONFile(
        objectFileImports,
        archiveImports,
        /* isBitcode */ true,
        bitcodeMergeState,
        /* loadedByLinker */
        loadedInputBitcodeFiles.contains(inputBitcodeFilePath),
        standaloneObjectFile->outputPlanFilePath);
  } else {
    co_await nonLTOObjectFilesMutex.co_lock();
    nonLTOObjectFiles.push_back(standaloneObjectFile->starlarkArrayIndex);
    nonLTOObjectFilesMutex.unlock();
    // The linker will not generate an index shard, or a merged bitcode file if
    // the input is not bitcode. Buck still expects the output, so write empty
    // files.
    std::ofstream(standaloneObjectFile->outputIndexShardFilePath).close();

    if (enablePreMerger) {
      std::ofstream(
          standaloneObjectFile->outputPremergedBitcodeFilePath.value())
          .close();
    }

    writeStandaloneObjectFileOptimizationPlanToJSONFile(
        /* objectFileImports */ {},
        /* archiveImports */ {},
        /* isBitcode */ false,
        // Native object files do no participate in bitcode merging
        /* bitcodeMergeState */ std::nullopt,
        // The native object file might not actually be loaded by the linker,
        // but it doesn't matter. The point of this field is to avoid optimizing
        // + codegening a bitcode file that won't be loaded anyways, but this is
        // already a native object file, there is no work to avoid doing.
        /* loadedByLinker */ true,
        standaloneObjectFile->outputPlanFilePath);
  }
  co_return;
}

} // namespace

int main(int argc, char** argv) {
  int fakeArgc = 1;
  folly::Init init(&fakeArgc, &argv);
  parseArgs(argc, argv);
  ObjectFileRecordsMapTy objectFileRecordsMap;
  std::unordered_map<int, std::unique_ptr<Archive>> archiveMap;
  parseMetaFileRecords(objectFileRecordsMap, archiveMap);
  std::unordered_map<std::string, std::string> preMergeToPostMerge;
  std::unordered_map<std::string, std::string> postMergeToPreMerge;
  if (enablePreMerger) {
    populatePreMergerPathConversionMaps(
        objectFileRecordsMap, preMergeToPostMerge, postMergeToPreMerge);
  }
  runThinLink(preMergeToPostMerge);

  std::unordered_set<std::string> loadedInputBitcodeFiles =
      parseLoadedInputBitcodeFiles();

  std::unordered_set<std::string> absorbedBitcodeFiles;
  std::vector<int> nonLTOObjectFiles;
  std::vector<folly::coro::Task<void>> tasks;
  for (const auto& [inputBitcodeFilePath, objectFileRecord] :
       objectFileRecordsMap) {
    const auto* standaloneObjectFile =
        dynamic_cast<const StandaloneObjectFile*>(objectFileRecord.get());
    if (!standaloneObjectFile) {
      continue;
    }

    tasks.push_back(writeOptimizationPlanForStandaloneObjectFile(
        inputBitcodeFilePath,
        standaloneObjectFile,
        objectFileRecordsMap,
        postMergeToPreMerge,
        loadedInputBitcodeFiles,
        absorbedBitcodeFiles,
        nonLTOObjectFiles));
  }

  folly::coro::blockingWait(co_withExecutor(
      folly::getGlobalCPUExecutor(),
      folly::coro::collectAllRange(std::move(tasks))));

  // We don't bother multithreading this part, since there are far fewer
  // pre-built static archives and they usually don't contain bitcode (so there
  // is less work to do reading and writing artifacts)
  for (const auto& [starlarkIndex, archive] : archiveMap) {
    std::filesystem::create_directories(
        archive->outputIndexShardsDirectoryPath);
    if (enablePreMerger) {
      std::filesystem::create_directories(
          archive->outputPremergedBitcodeDirectoryPath.value());
    }

    std::vector<ExtractedObjectFileOptimizationPlan> objectPlans;
    for (const auto& extractedObjectFile : archive->objects) {
      const std::string importsFilePath =
          thinLTOPrefixReplacedPath(extractedObjectFile->inputObjectFilePath)
              .concat(IMPORTS_FILE_SUFFIX);
      if (std::filesystem::exists(importsFilePath)) {
        const std::filesystem::path temporaryIndexShardFilePath =
            thinLTOPrefixReplacedPath(extractedObjectFile->inputObjectFilePath)
                .concat(SUMMARY_SHARD_SUFFIX);
        const std::string finalIndexShardFilePath =
            std::filesystem::path(
                extractedObjectFile->outputIndexShardsDirectoryPath) /
            temporaryIndexShardFilePath.filename();
        std::filesystem::rename(
            temporaryIndexShardFilePath, finalIndexShardFilePath);

        const auto imports =
            readImportsFile(importsFilePath, postMergeToPreMerge);
        std::vector<int> objectFileImports;
        std::vector<int> archiveImports;
        for (const auto& importPath : imports) {
          const auto objectFileRecord = objectFileRecordsMap.at(importPath);
          if (const auto* standaloneObjectFile =
                  dynamic_cast<const StandaloneObjectFile*>(
                      objectFileRecord.get())) {
            objectFileImports.push_back(
                standaloneObjectFile->starlarkArrayIndex);
          } else if (
              const auto* importedExtractedObjectFile =
                  dynamic_cast<const ExtractedObjectFile*>(
                      objectFileRecord.get())) {
            archiveImports.push_back(
                importedExtractedObjectFile->starlarkArrayIndex);
          }
        }

        std::optional<BitcodeMergeState> bitcodeMergeState;
        std::optional<std::string> mergedBitcodeFilePath;
        if (enablePreMerger) {
          mergedBitcodeFilePath =
              preMergeToPostMerge.at(extractedObjectFile->inputObjectFilePath);
          bitcodeMergeState =
              readMergedBitcodeFile(mergedBitcodeFilePath.value());
          if (bitcodeMergeState == BitcodeMergeState::Absorbed) {
            absorbedBitcodeFiles.insert(
                extractedObjectFile->inputObjectFilePath);
          }
        }

        objectPlans.push_back(ExtractedObjectFileOptimizationPlan(
            objectFileImports,
            archiveImports,
            /* isBitcode */ true,
            /* path */ extractedObjectFile->inputObjectFilePath,
            /* indexShardFilePath */ finalIndexShardFilePath,
            /* loadedByLinker */
            loadedInputBitcodeFiles.contains(
                extractedObjectFile->inputObjectFilePath),
            bitcodeMergeState,
            mergedBitcodeFilePath));
      } else {
        objectPlans.push_back(ExtractedObjectFileOptimizationPlan(
            /* objectFileImports*/ {},
            /* archiveImports */ {},
            /* isBitcode */ false,
            /* path */ extractedObjectFile->inputObjectFilePath,
            /* indexShardFilePath */ std::nullopt,
            /* loadedByLinker */ true,
            /* bitcodeMergeState */ std::nullopt,
            /* mergedBitcodeFilePath */ std::nullopt));
      }
    }

    writeArchiveOptimizationPlanTOJSONFile(
        objectPlans,
        /* baseDir */
        std::filesystem::path(archive->outputPlanFilePath).parent_path(),
        archive->outputPlanFilePath);
  }
  writeLinkPlan(nonLTOObjectFiles);
  writeFinalLinkIndex(absorbedBitcodeFiles, objectFileRecordsMap);
}
