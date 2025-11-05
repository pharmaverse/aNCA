Thank you for your interest in the aNCA application! We strive to provide open-source tooling for performing Non-Compartmental Analysis for both pre-clinical and clinical datasets, aiming to make NCA straightforward and approachable to all scientists.

## Who this document is for — roles at a glance

This CONTRIBUTING guide is organized by common contributor roles. Start at the role that best fits you and follow the short pointers below; each role links to more detailed guidance in the rest of this file.

- [User](#users-computer_mouse) — quick feedback and issue reporter: open issues when you find bugs or UX problems, and follow the simple reproduction checklist.
- [Developer](#developers-computer) — contributes code and features: follow branching rules, code style, tests and PR guidance.
- [NCA‑reviewer](#nca-reviewers-book) — domain reviewer of outputs and validations: focus on data/mapping/validation sections and how to reproduce runs via exported scripts.
- [Code‑reviewer](#code-reviewers-mag) — reviews PRs and enforces quality: focus on tests, linters, and the reviewers checklist.

Please, consider that all contributors should follow the general principles of respect and collaboration indicated in the [CODE_OF_CONDUCT.md](./CODE_OF_CONDUCT.md).
---

## Report bugs and suggest enhancements
Discussions regarding the project are handled using GitHub issues. If you wish to create a bug report, suggest changes and additions, or just ask questions regarding the project, please open a new issue. Templates for specific topics are available — try to select the appropriate one and follow its guidance.

In general, any posted issue should contain a brief description of the problem, steps to reproduce, and your expected outcome. We design this application with user-friendliness in mind and appreciate clear, actionable feedback.

## Users :computer_mouse:
This short section is for people who primarily use the app and want to report bugs, request features, or get help reproducing results.

- You find something is not intuitive or you don't know if something can or not be done? Check in [discussions](https://github.com/pharmaverse/aNCA/discussions) or in our [FAQs](). If you still cannot answer your question, open a [Q&A dicussion](https://github.com/pharmaverse/aNCA/discussions/new?category=q-a) and the community will help you!
- If you find a bug: open an [bug issue](https://github.com/pharmaverse/aNCA/issues) (if it does not already exist) and include a minimal reproduction (explain the steps you followed), the app version (see the footer in the UI) and screenshots or console logs if helpful. Please, make sure to not provide any sensitive or confidential data.
- If you want a new feature or improvement: open a [discussion for ideas](https://github.com/pharmaverse/aNCA/discussions/new?category=ideas) (if it does not already exist)  titled  `Feature request: ...`. Explain your use case and desired outcome, with as much detail as possible.

You can keep track and participate of other issues and discussions using the GitHub interface.

---

## Developers :computer:
If you plan to contribute code, tests, or documentation, please follow these developer-focused instructions.

- Join the project by asking anca.pharmaverse@gmail.com and request collaborator access if you need push rights; otherwise, fork and open a PR.
- You can find in our [project dashboard](https://github.com/orgs/pharmaverse/projects/30) our issues and their priority.
- Preferably, work on issues with higher priority (`MUST` > `SHOULD` > `COULD` > `WISH`). Please, before selecting MUST issues, make sure you will have enough time to complete them in a quick timeframe. If you are a first time contributor, consider selecting issues with the `good first issue` label.
- Branch naming: use a branch per issue if possible with a standard naming (e.g. `<issue number>-<bug/feat/documentation><issue name>`). For examples, see the [Branches](#branches) section below.
- Code style: follow the [tidyverse style](https://style.tidyverse.org/files.html); ensure lintr passes and add unit tests for new behavior.
- Commit messages: try to follow the [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/), although we are flexible about it (just make your best to name your changes intuitively for others!).
- Write a Pull Request (PR): Once you are done with your implementation, complete the default template that will be provided when opening a PR. Always include at least the number of the issue you intend to solve, a brief description of the changes made, and a checklist of testing steps to validate the changes.
- Pull requests: Make sure you pass all the Developers checklist (lintr, tests, docs, version bump). Add at least two reviewers from the core team (authors listed in the [DESCRIPTION file](../DESCRIPTION)). If you need help, just ask in the issue thread and tag any of the core team.
- Review feedback: Reviewers will provide feedback on your PR. Please address their comments, make necessary changes, and engage in discussions to ensure the quality of the contribution. 
- Review accepted: Congratulations! Once your PR is approved by at least two core reviewers, If your code is not simply a documentation or test change, you need to bump the package version. This simply consists in updating  the version number of your DESCRIPTION file in comparison with the [DESCRIPTION from the main branch](./DESCRIPTION). This is called [semantic versioning](https://semver.org/)
- Merge: Someone from the core team will merge your PR once everything is set. Thanks in advance for the changes!

Helpful links for Developers:
- Check the [design.md](./design.md) to understand better how the Shiny App is structured.
- If you need more context on what is Non Comparmental Analysis, we also recommend you to check [background.md](./background.md)

---

## NCA reviewers :book:
This role focuses on validating calculations, outputs, and the scientific correctness of NCA results.

- Copy the branch associated with the Pull Request (PR), pull it locally, and run `devtools::load_all(); aNCA::run_app()` to test the changes in the App.
- Make sure all `Definition of Done` items in the Pull Request (PR) are met for the Application.
- If there are any calculations or outputs, inspect them for full scientific correctness (accurate calculations, correct CDISC standards...)
- When relevant, provide feedback on how intuitive the new feature/enhancement is

Helpful links for NCA reviewers:
- When reviewing a pull request, please do try to follow the [conventional comments](https://conventionalcomments.org/) guidelines. Ideas and labels described in that convention can be very helpful in getting your thoughts across and facilitate meaningful cooperation. That said, they are not applicable in every circumstance and you are free to do whatever you feel is suitable, as long as it aims to provide valid discussion.

---

## Code reviewers :mag:
This role is responsible for reviewing PRs, enforcing quality, and approving merges.

- Copy the branch associated with the Pull Request (PR), pull it locally, and run `devtools::load_all(); aNCA::run_app()` to test the changes in the App.
- Focus reviews on code quality, correctness, and maintainability. Ensure new code included in `\R` has logical unit tests.
- Use the PR template and the Developers checklist before approving; request changes when standards are not met.
- Make sure the package version is bumped for changes that affect behavior or dependencies.

Helpful links for Code reviewers:
- When reviewing a pull request, please do try to follow the [conventional comments](https://conventionalcomments.org/) guidelines. Ideas and labels described in that convention can be very helpful in getting your thoughts across and facilitate meaningful cooperation. That said, they are not applicable in every circumstance and you are free to do whatever you feel is suitable, as long as it aims to provide valid discussion.



# Beware
- The package is split into two parts: the R package and a shiny application. Logic code, which is useful on its own and can be easily run via the console should be placed in the `R/` directory. Code that is strictly related to **Shiny** application, especially code that generates some interface elements, should be placed in `inst/shiny/` folder. 
- The package requires some dependencies for which there are no explicit `@import` statements in the `NAMESPACE` file. This is because some of those functions are not used in the **package** itself, but rather in the **Shiny application**. Please beware of this situation when managing the package dependencies - some functions might be used in the app, even though they are not verbosely imported.
- For development purposes, you can specify a log level using `aNCA_LOG_LEVEL` environment variable. As a default, this is set to **INFO**. We recommend setting the log level to **TRACE**, e.g. in your `.Renviron` file (`aNCA_LOG_LEVEL="TRACE"`).

# In-depth guides
Here are some useful links with in-depth documentation regarding specific parts of the pacakage and how to utilise in-build tools to extend the capabilities of the application:

- [Adding TLGs](https://pharmaverse.github.io/aNCA/articles/adding_tlg.html)
