// Copyright 2004-present Facebook. All Rights Reserved.

#include "buck2/tests/targets/rules/cxx/linking/any.h"
#include "buck2/tests/targets/rules/cxx/linking/shared.h"
#include "buck2/tests/targets/rules/cxx/linking/shared2.h"
#include "buck2/tests/targets/rules/cxx/linking/static.h"

int main() {
  any();
  shared();
  shared2();
  Static();
}
