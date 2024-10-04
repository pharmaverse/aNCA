Thank you for your interest in the aNCA application! We strive to provide open-sourced for performing Non-Compartment Analysis for both pre-clinical and clinical datasets, aiming to make NCA straightforward and approachable to all scientists.

# Report bugs and suggest enhancements
Discussions regarding the project are handled using issues system directly on GitHub. If you wish to create a bug report, suggest changes and additions, or just ask questions regarding the project feel free to open a new issue. Templates for specific topic are available, so please try to select the appropriate one and follow the guidelines as close as applicable.

In general, any posted issue should contain a brief description of the problem, as well as your expectations - we design this application with user-friendliness in mind, we wish to hear your opinion!

# Submit changes
## Issues
Before creating any changes, please make sure an appropriate issue thread is opened. This is the place to discuss the topic at hand, agree on the approach for solving the problem and keep the tasks organized across the whole project.

## Branches
Each change should be implemented on a separate branch. Branches should have related issues opened and be named after the issue topic. Branch name should include change type (bug/enhancement/documentation etc.) and name of the solved issue. Examples:

| Issue                                   | Branch                                 |
|-----------------------------------------|----------------------------------------|
| Documentation: add CONTRIBUTING.md file | documentation/add-contributing-md-file |
| Bug: help widget not working            | bug/help-widget-not-working            |
|                                         |                                        |

## Pull requests

[Pull request template](.github/PULL_REQUEST_TEMPLATE.md) is available to make documenting PRs more consistent and streamlined. Each PR description should include:

#### Issue
Number of the appropriate issue the pull request closes or references.

#### Description
Brief description of implemented changes, what they do and what is the reasoning behind the changes. 

#### Definition of Done
Checklist with minimal requirements to consider the feature or bugfix complete. Preferably the list supplied with the appropriate issue. Requirements might change and the scope might grow or shrink during development, so feel free to make changes to the list if relevant, but make sure any deviations are documented.

#### How to test
Instructions on how to test the new feature manually (go to..., click on...). Might include code snippets. It is especially important to include such instructions if submitted code is not fully covered by unit tests. If full suite of unit tests is supplied alongside logic, this part might not be required or relevant in terms of miscellaneous changes (documentation, dependencies etc.).

#### Developers checklist
This part is here as a reminder to perform basic tasks and checks before the code is submitted, to ensure compliance with the guidelines. Before opening a PR for review, please make sure that:
- Code passes lintr checks
- Code passes all unit tests
- New logic covered is by unit tests
- New logic is documented

The above rules will help keep our work organized, as well as allow for quick information flow between related issues, branches and pull requests.

# Code guidelines
TBA: depends on implemented rules within the `lintr` package.

# Code review
TBA