# Content
1. `src/Dictionary.sml` - custom Dictionary structure to be tested.
1. `tests/DictionaryTests.sml` - the structure that tests Dictionary using *CakeML-Testlib* framework.
1. `tests/Makefile` - file to specify how to build the example with testing framework.

# How to run
1. Ensure that the path to CakeML compiler specified in Makefile corresponds to actual compiler path.
1. First files specified in the `TEST_UNIT_LIST` should always point to source code files of CakeML-Testlib specified in the same order as in the main Makefile.
1. Add paths to a file with a structure to be tested, following the file with tests.
1. Run `make` command in your terminal.