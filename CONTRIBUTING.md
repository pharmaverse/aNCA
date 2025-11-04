Thank you for your interest in the aNCA application! We strive to provide open-source tooling for performing Non-Compartmental Analysis for both pre-clinical and clinical datasets, aiming to make NCA straightforward and approachable to all scientists.

## Who this document is for — roles at a glance

This CONTRIBUTING guide is organized by common contributor roles. Start at the role that best fits you and follow the short pointers below; each role links to more detailed guidance in the rest of this file.

- User — quick feedback and issue reporter: open issues when you find bugs or UX problems, and follow the simple reproduction checklist. (See: #users)
- Developer — contributes code and features: follow branching rules, code style, tests and PR guidance. (See: #developers)
- NCA‑reviewer — domain reviewer of outputs and validations: focus on data/mapping/validation sections and how to reproduce runs via exported scripts. (See: #nca-reviewers)
- Code‑reviewer — reviews PRs and enforces quality: focus on tests, linters, and the reviewers checklist. (See: #code-reviewers)

Scroll to the role that matches your contribution and use the links there for the relevant sections.

## Quick links

- Users: jump to <a name="users"></a>Users
- Developers: jump to <a name="developers"></a>Developers
- NCA reviewers: jump to <a name="nca-reviewers"></a>NCA reviewers
- Code reviewers: jump to <a name="code-reviewers"></a>Code reviewers

---

## Report bugs and suggest enhancements
Discussions regarding the project are handled using GitHub issues. If you wish to create a bug report, suggest changes and additions, or just ask questions regarding the project, please open a new issue. Templates for specific topics are available — try to select the appropriate one and follow its guidance.

In general, any posted issue should contain a brief description of the problem, steps to reproduce, and your expected outcome. We design this application with user-friendliness in mind and appreciate clear, actionable feedback.

## Users
This short section is for people who primarily use the app and want to report bugs, request features, or get help reproducing results.

- If you find a bug: open an [bug issue](https://github.com/pharmaverse/aNCA/issues) (if it does not already exist) and include a minimal reproduction (explain the steps you followed), the app version (see the footer in the UI) and screenshots or console logs if helpful. Please, make sure to not provide any sensitive or confidential data.
- If you want a new feature or improvement: open a [discussion for ideas](https://github.com/pharmaverse/aNCA/discussions/new?category=ideas) (if it does not already exist)  titled  `Feature request: ...`. Explain your use case and desired outcome, with as much detail as possible.

You can keep track and participate of other issues and discussions using the GitHub interface.

---

## Developers :computer:
If you plan to contribute code, tests, or documentation, please follow these developer-focused instructions.

- Join the project and request collaborator access if you need push rights; otherwise, fork and open a PR.
- Always open an issue describing the change before implementing it so the work can be coordinated.
- Branch naming: use a branch per issue and follow the examples in this file (e.g. `bug/...`, `enhancement/...`).
- Code style: follow the tidyverse style; ensure lintr passes and add unit tests for new behavior.
- Commit messages: prefer Conventional Commits for clarity.
- Pull requests: fill the PR template, list the issue number, description, testing steps, and include a Developers checklist (lintr, tests, docs, version bump).

Helpful links for Developers:
- See the `Branch` and `Pull requests` sections below for full guidance.
- Check the [design.md](./design.md) to understand better how the Shiny App is structured.
- If you need more context on what is Non Comparmental Analysis, we also recommend you to check [background.md](./background.md)

---

## NCA reviewers
This role focuses on validating calculations, outputs, and the scientific correctness of NCA results.

- Make sure all `Definition of Done` items in the Pull Request (PR) are met.
- If there are any calculations or outputs, inspect them for full scientific correctness (accurate calculations, correct CDISC standards...)
- When relevant, provide feedback on how intuitive the new feature/enhancement is

Helpful links for NCA reviewers:
- See the `Branch` and `Pull requests` sections below for full guidance. You need to learn how to copy the branch associated to a Pull Request (PR), pull it locally, and run the code to validate the changes.

---

## Code reviewers
This role is responsible for reviewing PRs, enforcing quality, and approving merges.

- Focus reviews on tests, correctness, and maintainability. Ensure new code has unit tests and passes linters.
- Use the PR template and the Developers checklist before approving; request changes when standards are not met.
- Make sure the package version is bumped for changes that affect behavior or dependencies.

Helpful links for Code reviewers:
- See the `Code review`, `Pull requests`, and `Bumping package version` sections below.


# Submit changes
## Joining the project
In order to gain privileges to commit changes to the codebase, you will need to be added as a collaborator. To do so, please contact one of the contributors. You can find the list with contact info in the [DESCRIPTION](/DESCRIPTION) file.

## Issues
Before creating any changes, please make sure an appropriate issue thread is opened. This is the place to discuss the topic at hand, agree on the approach for solving the problem and keep the tasks organized across the whole project.

To keep things organized, [GitHub Project board](https://github.com/orgs/pharmaverse/projects/30/views/1) is used.

#### Status
Each new issue should go to **Backlog**. Feel free to add not only any bugs and feature requests, but also loose ideas - this is also a space for discussing proposals and validity of implementing them. When committed to implementing a change, the issue is moved to `Todo` status, and subsequently to `In progress` when a feature branch is created and work has begun. When the change is ready and the pull request is open, issues should be moved to `Needs review` where it will be picked up by a reviewer and moved to `In review`. After the change is merged and issue is closed, it will be moved to `Done`.

#### Priority
The tasks are prioritized based on a variant of the [MSCW method](https://en.wikipedia.org/wiki/MoSCoW_method). It gives a verbose descriptions for each level of priority:
- **MUST** - task of highest priority, must be implemented for the application to be usable and useful.
- **SHOULD** - task that we should include in the ready product, but it will not be breaking if it is not implemented.
- **COULD** - things that would be nice to have and useful, but not instrumental to the whole package.
- **WISH** - ideas that are not valid for the time being, but are nevertheless cool and could become useful as the development progresses.

## Branches
Each change should be implemented on a separate branch. Branches should have related issues opened and be named after the issue topic. Branch name should include change type (bug/enhancement/documentation etc.) and name of the solved issue. Examples:

| Issue                                   | Branch                                 |
|-----------------------------------------|----------------------------------------|
| Documentation: add CONTRIBUTING.md file | documentation/add-contributing-md-file |
| Bug: help widget not working            | bug/help-widget-not-working            |
|                                         |                                        |

## Code guidelines
The codebase follows the general [tidyverse](https://style.tidyverse.org/files.html) guidelines, but with lenient implementation. Please do make an effort to make your code clean, readable and easily understandable for the reviewer. In general, as long as your code passes all the `lintr` tests, you are good.

## Commit messages
Do try to follow the [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/) convention whenever possible. That said, software development is messy and if you feel like this standard does not suit the situation, feel free to deviate from it, but do keep the commit messages short and meaningful at the very least.

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
- Package version is bumped

The above rules will help keep our work organized, as well as allow for quick information flow between related issues, branches and pull requests.

#### Notes to reviewer
If there is anything that the reviewer should know before tackling the pull request, please provide it here. This could include things like pointing to specific parts of the code that require special attention, explaining decisions behind unusual implementations or providing logic behind changing the scope of the task.

### Bumping package version
Package versioning must use the semantic versioning schema, with developement extension: **X.Y.Z.9NNN**

#### Developement version
In each PR that introduces changes to the code, dependencies or the function documentation, it is required that the developement package version is incremented by 1 (this is the .9NNN number, the last part of the versioning schema). As the current feature branch might be outdated, before merging please cross-check with the **main** branch that the version you are submitting is correct.

#### Semantic versioning
The versioning schema for major / minor / patch releases must adhere to [semantic versioning](https://semver.org/) guidelines.

## Code review
Each pull request must be accepted by at least two reviewers before it can be merged to the main branch.

#### For reviewee
When the change is done, pull request is open and the description is filled, please move your issue from **In Progress** to **Needs review** status, so it can be picked up by a reviewer. From this point it is up to the contributor and the person validating the change to work out any kinks and lead to merging the changes.

#### For reviewers
When reviewing a pull request, please do try to follow the [conventional comments](https://conventionalcomments.org/) guidelines. Ideas and labels described in that convention can be very helpful in getting your thoughts across and facilitate meaningful cooperation. That said, they are not applicable in every circumstance and you are free to do whatever you feel is suitable, as long as it aims to provide valid discussion.

# Beware
- The package is split into two parts: the R package and a shiny application. Logic code, which is useful on its own and can be easily run via the console should be placed in the `R/` directory. Code that is strictly related to **Shiny** application, especially code that generates some interface elements, should be placed in `inst/shiny/` folder. 
- The package requires some dependencies for which there are no explicit `@import` statements in the `NAMESPACE` file. This is because some of those functions are not used in the **package** itself, but rather in the **Shiny application**. Please beware of this situation when managing the package dependencies - some functions might be used in the app, even though they are not verbosely imported.
- For development purposes, you can specify a log level using `aNCA_LOG_LEVEL` environment variable. As a default, this is set to **INFO**. We recommend setting the log level to **TRACE**, e.g. in your `.Renviron` file (`aNCA_LOG_LEVEL="TRACE"`).

# In-depth guides
Here are some useful links with in-depth documentation regarding specific parts of the pacakage and how to utilise in-build tools to extend the capabilities of the application:

- [Adding TLGs](https://pharmaverse.github.io/aNCA/articles/adding_tlg.html)