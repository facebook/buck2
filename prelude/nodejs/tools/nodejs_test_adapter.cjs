/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

'use strict';

// Runs the test files of a `nodejs_test` for the `js_test` external runner.
// Without `--write-test-results`, a `TEST_RESULTS_OUTPUT_FILE` inherited from
// an enclosing test is left alone.
//
// Loaded by `--test-reporter`, this file is instead the node:test reporter.

const childProcess = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {StringDecoder} = require('node:string_decoder');
const {pathToFileURL} = require('node:url');
const util = require('node:util');

const SEPARATOR = ' > ';
const PASS = 0;
const FAIL = 1;
const SKIP = 2;
const RUN_FAILURE_FATAL = 2;
// How much output a failed result keeps; all of it is still printed.
const MAX_OUTPUT_BYTES = 64 * 1024;

// Reported by its message alone, without a stack.
class AdapterError extends Error {}

function parseArgs(argv) {
  const options = {
    framework: undefined,
    files: [],
    filters: [],
    list: false,
    output: undefined,
    nodeArgs: [],
    writeTestResults: false,
  };
  let i = 0;
  const value = flag => {
    if (i >= argv.length) {
      throw new AdapterError(`${flag} requires a value`);
    }
    return argv[i++];
  };
  while (i < argv.length) {
    const arg = argv[i++];
    switch (arg) {
      case '--framework':
        options.framework = value(arg);
        break;
      case '--test-file':
        options.files.push({name: value(arg), path: value(arg)});
        break;
      case '--filter':
        options.filters.push(value(arg));
        break;
      case '--list':
        options.list = true;
        break;
      case '--output':
        options.output = value(arg);
        break;
      case '--node-arg':
        options.nodeArgs.push(value(arg));
        break;
      case '--write-test-results':
        options.writeTestResults = true;
        break;
      default:
        throw new AdapterError(`unexpected argument '${arg}'`);
    }
  }
  if (options.framework !== 'node') {
    throw new AdapterError(`unsupported --framework '${options.framework}'`);
  }
  return options;
}

function writeListing(files, output) {
  const listing = files
    .map(file => JSON.stringify({testcase: file.name}) + '\n')
    .join('');
  if (output === undefined) {
    process.stdout.write(listing);
  } else {
    fs.writeFileSync(output, listing);
  }
}

function escapeRegExp(text) {
  return text.replace(/[\\^$.*+?()[\]{}|]/g, '\\$&');
}

// A run is `whole` when there are no filters or a filter names its file; its
// `filters` name tests or `describe` blocks in it.
function selectRuns(files, filters) {
  if (filters.length === 0) {
    return files.map(file => ({file, whole: true, filters: []}));
  }
  const runsByFile = new Map();
  const runOf = file => {
    let run = runsByFile.get(file);
    if (run === undefined) {
      run = {file, whole: false, filters: new Set()};
      runsByFile.set(file, run);
    }
    return run;
  };
  for (const filter of filters) {
    const whole = files.find(file => file.name === filter);
    if (whole !== undefined) {
      runOf(whole).whole = true;
      continue;
    }
    let file;
    for (const candidate of files) {
      if (
        filter.startsWith(candidate.name + SEPARATOR) &&
        (file === undefined || candidate.name.length > file.name.length)
      ) {
        file = candidate;
      }
    }
    if (file === undefined) {
      throw new AdapterError(
        `unknown filter '${filter}': expected a test file name or '<test file name>${SEPARATOR}<test name>'`,
      );
    }
    runOf(file).filters.add(filter);
  }
  return files
    .filter(file => runsByFile.has(file))
    .map(file => {
      const run = runsByFile.get(file);
      return {file, whole: run.whole, filters: [...run.filters]};
    });
}

// node:test matches a name pattern against a test's name prefixed with its
// ancestors' names, separated by spaces. A separator in `filter` can also be
// part of a name, so the pattern accepts either.
function fullNamePattern(file, filter) {
  return filter
    .slice(file.name.length + SEPARATOR.length)
    .split(SEPARATOR)
    .map(escapeRegExp)
    .join(`(?:${SEPARATOR}| )`);
}

function namePattern(file, filter) {
  return `^${fullNamePattern(file, filter)}$`;
}

// Selects the results of what `namePattern` runs: the tests and suites it
// matches and everything inside them.
function resultNameRegExp(file, filter) {
  return new RegExp(
    `^${escapeRegExp(file.name + SEPARATOR)}${fullNamePattern(file, filter)}(?:${SEPARATOR}|$)`,
  );
}

function printNote(message) {
  process.stderr.write(`nodejs_test: ${message}\n`);
}

function filterResults(file, tests, suites, whole, filters, ended, endedTime) {
  if (filters.length === 0) {
    return tests;
  }
  const patterns = new Map(
    filters.map(filter => [filter, resultNameRegExp(file, filter)]),
  );
  const selects = (filter, name) => patterns.get(filter).test(name);
  const selected = whole
    ? tests
    : tests.filter(test => filters.some(filter => selects(filter, test.name)));
  if (!ended) {
    return selected;
  }
  const unmatched = filters
    .filter(
      filter =>
        !tests.some(test => selects(filter, test.name)) &&
        !suites.some(suite => selects(filter, suite)),
    )
    .map(filter => {
      printNote(`no test matched the filter '${filter}'`);
      return {
        name: filter,
        status: FAIL,
        message: 'no test matched this filter',
        duration: 0,
        endedTime,
      };
    });
  return [...selected, ...unmatched];
}

const children = new Set();

// Re-raising the signal ends this process without running `finally` blocks,
// so the handler removes `workDir` itself.
function killChildrenOnSignal(workDir) {
  for (const signal of ['SIGINT', 'SIGTERM']) {
    process.once(signal, () => {
      for (const child of children) {
        child.kill(signal);
      }
      fs.rmSync(workDir, {recursive: true, force: true});
      process.kill(process.pid, signal);
    });
  }
}

function linePrefixer(stream, prefix) {
  const decoder = new StringDecoder('utf8');
  let partial = '';
  const flush = text => {
    const lines = (partial + text).split('\n');
    partial = lines.pop();
    if (lines.length > 0) {
      stream.write(lines.map(line => `${prefix}${line}\n`).join(''));
    }
  };
  return {
    write: chunk => flush(decoder.write(chunk)),
    end: () => {
      flush(decoder.end());
      if (partial !== '') {
        stream.write(`${prefix}${partial}\n`);
        partial = '';
      }
    },
  };
}

function spawnNode(nodeArgs, args, prefix) {
  const started = Date.now();
  return new Promise(resolve => {
    const output = [];
    let outputBytes = 0;
    const keep = chunk => {
      output.push(chunk);
      outputBytes += chunk.length;
      while (outputBytes - output[0].length >= MAX_OUTPUT_BYTES) {
        outputBytes -= output.shift().length;
      }
    };
    const passThrough = stream =>
      prefix === undefined
        ? {write: chunk => stream.write(chunk), end: () => {}}
        : linePrefixer(stream, prefix);
    const stdout = passThrough(process.stdout);
    const stderr = passThrough(process.stderr);
    let settled = false;
    const outcome = fields => {
      if (settled) {
        return;
      }
      settled = true;
      stdout.end();
      stderr.end();
      resolve({
        code: null,
        signal: null,
        error: undefined,
        durationMs: Date.now() - started,
        output: Buffer.concat(output),
        ...fields,
      });
    };
    const child = childProcess.spawn(
      process.execPath,
      [...process.execArgv, ...nodeArgs, ...args],
      {stdio: ['ignore', 'pipe', 'pipe']},
    );
    children.add(child);
    child.stdout.on('data', chunk => {
      stdout.write(chunk);
      keep(chunk);
    });
    child.stderr.on('data', chunk => {
      stderr.write(chunk);
      keep(chunk);
    });
    child.on('error', error => {
      children.delete(child);
      outcome({error});
    });
    child.on('close', (code, signal) => {
      children.delete(child);
      outcome({code, signal});
    });
  });
}

function processDetails(command, outcome, describeExit, notes = []) {
  const lines = [...notes];
  if (outcome.error !== undefined) {
    lines.push(`could not run ${command}: ${outcome.error.message}`);
  } else if (outcome.signal !== null) {
    lines.push(`${command} was killed by ${outcome.signal}`);
  } else if (describeExit) {
    lines.push(`${command} exited with code ${outcome.code}`);
  }
  const {output} = outcome;
  if (output.length > 0) {
    let start = Math.max(0, output.length - MAX_OUTPUT_BYTES);
    // Skip UTF-8 continuation bytes.
    while (start < output.length && (output[start] & 0xc0) === 0x80) {
      start++;
    }
    const kept = output.subarray(start);
    lines.push(...(lines.length > 0 ? ['', kept.toString()] : [kept.toString()]));
  }
  return lines.join('\n');
}

function testFileResult(file, tests, details, duration, endedTime) {
  const failed = tests.filter(test => test.status === FAIL);
  const result = {name: file.name, duration, endedTime};
  if (details !== undefined || failed.length > 0) {
    const lines = failed.map(test => `failed: ${test.name}`);
    if (details !== undefined) {
      lines.push(...(lines.length > 0 ? ['', details] : [details]));
    }
    result.status = FAIL;
    result.message = lines.join('\n');
  } else if (tests.length > 0 && tests.every(test => test.status === SKIP)) {
    result.status = SKIP;
  } else {
    result.status = PASS;
  }
  return result;
}

function readEvents(eventsFile) {
  let text;
  try {
    text = fs.readFileSync(eventsFile, 'utf8');
  } catch (error) {
    if (error.code === 'ENOENT') {
      return {events: [], malformed: 0};
    }
    throw error;
  }
  // A crash can cut short the last event, which has no newline after it.
  const lines = text.split('\n');
  const events = [];
  let malformed = 0;
  for (const [index, line] of lines.entries()) {
    if (line === '') {
      continue;
    }
    try {
      events.push(JSON.parse(line));
    } catch {
      if (index !== lines.length - 1) {
        malformed++;
      }
    }
  }
  return {events, malformed};
}

// `unfinished` lists the innermost tests that started running but never
// reported, if the run did not end.
function nodeTestReport(file, events, endedTime) {
  const tests = [];
  const suites = [];
  let ended = false;
  const reporting = [];
  const running = [];
  const unfinished = [];
  const fullName = (ancestors, event) =>
    [file.name, ...ancestors.slice(0, event.nesting), event.name].join(
      SEPARATOR,
    );
  const location = event =>
    [event.file, event.line, event.column, event.nesting, event.name].join(
      ':',
    );
  for (const event of events) {
    if (event.type === 'test:plan') {
      ended = true;
      continue;
    }
    if (event.type === 'test:dequeue') {
      // Tests with `concurrency` interleave, so these ancestors are a guess.
      running.length = event.nesting;
      running.push(event.name);
      unfinished.push({
        name: fullName(running, event),
        location: location(event),
      });
      continue;
    }
    if (event.type === 'test:start') {
      reporting.length = event.nesting;
      reporting.push(event.name);
      continue;
    }
    const name = fullName(reporting, event);
    const eventLocation = location(event);
    const index = unfinished.findLastIndex(
      entry => entry.location === eventLocation,
    );
    if (index !== -1) {
      unfinished.splice(index, 1);
    }
    // A suite or a test with subtests fails when a subtest does, and that
    // subtest has its own result. A suite that passes has results for its
    // tests, unless it was skipped and they never ran.
    if (event.failureType === 'subtestsFailed') {
      continue;
    }
    if (event.suite && event.type === 'test:pass' && !event.skip && !event.todo) {
      suites.push(name);
      continue;
    }
    const result = {
      name,
      duration: Math.round(event.durationMs ?? 0),
      endedTime: event.endedTime ?? endedTime,
    };
    // `skip` and `todo` are `true` or the reason.
    const skipped = event.skip || event.todo;
    if (skipped) {
      result.status = SKIP;
      if (typeof skipped === 'string') {
        result.message = skipped;
      }
    } else if (event.type === 'test:fail') {
      result.status = FAIL;
      result.message = event.error;
    } else {
      result.status = PASS;
    }
    tests.push(result);
  }
  return {
    tests,
    suites,
    ended,
    unfinished: ended
      ? []
      : unfinished
          .map(entry => entry.name)
          .filter(
            (name, _, names) =>
              !names.some(other => other.startsWith(name + SEPARATOR)),
          ),
  };
}

// Runs `node <file>`, not `node --test`, whose positional arguments are globs:
// a path containing `[...]`, `{...}` or `@(...)` would run nothing and pass.
async function runNodeTestFile(options, run, eventsFile, prefix) {
  const outcome = await spawnNode(options.nodeArgs, [
    `--test-reporter=${pathToFileURL(__filename).href}`,
    `--test-reporter-destination=${eventsFile}`,
    '--test-reporter=spec',
    '--test-reporter-destination=stdout',
    ...(run.whole ? [] : run.filters).map(
      filter => `--test-name-pattern=${namePattern(run.file, filter)}`,
    ),
    run.file.path,
  ], prefix);
  const endedTime = Date.now();
  let events = [];
  let malformed = 0;
  let readError;
  try {
    ({events, malformed} = readEvents(eventsFile));
  } catch (error) {
    readError = error;
  }
  const report = nodeTestReport(run.file, events, endedTime);
  const tests = filterResults(
    run.file,
    report.tests,
    report.suites,
    run.whole,
    run.filters,
    report.ended,
    endedTime,
  );
  const failed = tests.some(test => test.status === FAIL);
  const crashed = outcome.error !== undefined || outcome.code !== 0;
  const notes = report.unfinished.map(name => `did not finish: ${name}`);
  if (readError !== undefined) {
    notes.push(`could not read the node:test events: ${readError.message}`);
  } else if (!report.ended && !crashed) {
    notes.push(
      'node:test did not report the end of the run: the file defines no tests, or it exited before they finished (by calling `process.exit()`, for example)',
    );
  }
  if (malformed > 0) {
    notes.push(`${malformed} node:test event(s) were malformed and dropped`);
  }
  for (const note of notes) {
    printNote(`${run.file.name}: ${note}`);
  }
  const details =
    failed || crashed || notes.length > 0
      ? processDetails('node', outcome, !failed, notes)
      : undefined;
  return [
    ...tests,
    testFileResult(run.file, tests, details, outcome.durationMs, endedTime),
  ];
}

async function runNodeTests(options, runs, workDir) {
  // Before Node 22, a name pattern matches a test's own name only, so it
  // cannot name a test inside a describe block.
  const [major] = process.versions.node.split('.').map(Number);
  const unsupported = run => major < 22 && !run.whole && run.filters.length > 0;
  // The default concurrency of `node --test`.
  const parallelism = Math.min(
    runs.length,
    Math.max(1, (os.availableParallelism?.() ?? os.cpus().length) - 1),
  );
  const resultsByRun = [];
  let next = 0;
  const worker = async () => {
    while (next < runs.length) {
      const index = next++;
      const run = runs[index];
      if (unsupported(run)) {
        const details = `filtering by test or describe block (${run.filters.join(', ')}) needs Node 22 or later, and this is Node ${process.versions.node}; filter by test file instead`;
        printNote(`${run.file.name}: ${details}`);
        resultsByRun[index] = [
          testFileResult(run.file, [], details, 0, Date.now()),
        ];
        continue;
      }
      try {
        resultsByRun[index] = await runNodeTestFile(
          options,
          run,
          path.join(workDir, `${index}.jsonl`),
          parallelism > 1 ? `[${run.file.name}] ` : undefined,
        );
      } catch (error) {
        const details = `the adapter failed while running this file: ${error?.stack ?? error}`;
        printNote(`${run.file.name}: ${details}`);
        resultsByRun[index] = [
          testFileResult(run.file, [], details, 0, Date.now()),
        ];
      }
    }
  };
  await Promise.all(Array.from({length: parallelism}, worker));
  return resultsByRun.flat();
}

function writeTestResults(events) {
  const output = process.env.TEST_RESULTS_OUTPUT_FILE;
  if (!output) {
    return;
  }
  fs.mkdirSync(path.dirname(output), {recursive: true});
  fs.writeFileSync(
    output,
    events.map(event => JSON.stringify(event) + '\n').join(''),
  );
}

function resultEvents(result) {
  const finish = {
    name: result.name,
    status: result.status,
    ended_time: result.endedTime,
    duration: result.duration,
  };
  if (result.message !== undefined) {
    finish.message = result.message;
  }
  return [
    {start: {name: result.name, started_time: result.endedTime - result.duration}},
    {finish},
  ];
}

function runFailureEvent(details) {
  return {run_failure: {status: RUN_FAILURE_FATAL, time: Date.now(), details}};
}

async function main(argv) {
  const options = parseArgs(argv);
  if (options.list) {
    writeListing(options.files, options.output);
    return {code: 0};
  }
  const runs = selectRuns(options.files, options.filters);
  const workDir = fs.mkdtempSync(path.join(os.tmpdir(), 'nodejs_test-'));
  killChildrenOnSignal(workDir);
  let results;
  try {
    results = await runNodeTests(options, runs, workDir);
  } finally {
    fs.rmSync(workDir, {recursive: true, force: true});
  }
  return {
    code: results.some(result => result.status === FAIL) ? 1 : 0,
    events: options.writeTestResults ? results.flatMap(resultEvents) : undefined,
  };
}

function describeError(error) {
  const {cause} = error;
  if (typeof cause?.stack !== 'string') {
    return error.message;
  }
  const inspected = util.inspect(cause);
  return error.failureType === 'testCodeFailure'
    ? inspected
    : `${error.message}\n${inspected}`;
}

async function* recordEvents(source) {
  for await (const {type, data} of source) {
    if (type === 'test:dequeue' || type === 'test:start') {
      yield JSON.stringify({
        type,
        name: data.name,
        nesting: data.nesting,
        file: data.file,
        line: data.line,
        column: data.column,
      }) + '\n';
    } else if (type === 'test:plan' && data.nesting === 0) {
      yield JSON.stringify({type}) + '\n';
    } else if (type === 'test:pass' || type === 'test:fail') {
      const {error} = data.details;
      yield JSON.stringify({
        type,
        name: data.name,
        nesting: data.nesting,
        file: data.file,
        line: data.line,
        column: data.column,
        suite: data.details.type === 'suite',
        durationMs: data.details.duration_ms,
        endedTime: Date.now(),
        skip: data.skip,
        todo: data.todo,
        failureType: error?.failureType,
        error: error == null ? undefined : describeError(error),
      }) + '\n';
    }
  }
}

if (require.main === module) {
  const argv = process.argv.slice(2);
  main(argv)
    .catch(error => {
      const details =
        error instanceof AdapterError ? error.message : util.inspect(error);
      console.error(`nodejs_test: ${details}`);
      return {
        code: 2,
        events: argv.includes('--write-test-results')
          ? [runFailureEvent(details)]
          : undefined,
      };
    })
    .then(({code, events}) => {
      process.exitCode = code;
      if (events === undefined) {
        return;
      }
      try {
        writeTestResults(events);
      } catch (writeError) {
        console.error(
          `nodejs_test: failed to write test results: ${util.inspect(writeError)}`,
        );
        process.exitCode = 2;
      }
    });
} else {
  module.exports = recordEvents;
}
