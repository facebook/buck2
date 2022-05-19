# @generated
# Copyright 2017 The Bazel Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Some definition to setup jenkins and build the corresponding docker images
load(":jenkins_docker_build.bzl", "jenkins_docker_build")
load(":jenkins_node.bzl", "jenkins_node")
load(":jenkins_nodes.bzl", "jenkins_nodes", "jenkins_node_names")
load(":jenkins_job.bzl", "jenkins_job", "bazel_git_job", "bazel_github_job")
