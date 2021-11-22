
# Parsing SPEC.md

###### specs

- When encountering an h1 node...
  - [ ] as soon as a test item is encountered, a new module will be created
  - [ ] if non-test item nodes follow the module, they will be included as comments at the top of the generated file
- When encountering an h6 named "specs", all checkboxes afterwards become test cases
