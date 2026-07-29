# Security Policy

## Supported Versions

Security updates are provided for the most recent release of aNCA. We
recommend always using the latest version available on
[CRAN](https://CRAN.R-project.org/package=aNCA) or the current
development version from the [`main`
branch](https://github.com/pharmaverse/aNCA).

| Version        | Supported |
|----------------|-----------|
| Latest release | ✅        |
| Older versions | ❌        |

## Reporting a Vulnerability

We take the security of aNCA seriously. If you believe you have found a
security vulnerability, please report it privately so we can address it
before it is publicly disclosed.

**Please do not report security vulnerabilities through public GitHub
issues, discussions, or pull requests.**

Instead, use one of the following private channels:

- **GitHub Private Vulnerability Reporting** (preferred): open a report
  via the [Security
  tab](https://github.com/pharmaverse/aNCA/security/advisories/new) of
  this repository.
- **Email**: contact the maintainers at
  **<anca.pharmaverse@gmail.com>**.

Please include as much of the following as you can:

- A description of the vulnerability and its potential impact.
- Steps to reproduce, or a proof of concept.
- The version of aNCA affected.
- Any suggested mitigation, if known.

## Response Process

- We will acknowledge your report within **14 days**.
- We will investigate and keep you informed of our progress.
- Once resolved, we will coordinate disclosure and credit you for the
  report, unless you prefer to remain anonymous.

## Scope

This policy applies to the aNCA package and its Shiny application. As
aNCA processes user-supplied pharmacokinetic datasets locally, please be
mindful **not to include any sensitive, confidential, or patient data**
in your reports.

Vulnerabilities in aNCA’s dependencies (e.g. `PKNCA`, `shiny`) should be
reported to their respective maintainers, though we appreciate being
informed if they affect aNCA.
