# Contributing to the Codebook Generator App

Thank you for your interest in contributing to the Codebook Generator App. This project aims to support the creation of structured, reusable, and FAIR-aligned codebooks for tabular research data. Whether you're reporting bugs, suggesting improvements, or writing code, your contributions are highly valued.

## How to Contribute

### 🐞 Report Bugs

If you encounter a bug, please open an issue describing:

* The steps to reproduce the problem.
* The behavior you expected.
* The actual behavior observed.
* Any relevant files (e.g., input data or screenshots).

### 💡 Suggest Features

If you have ideas for new features or user interface improvements, feel free to submit an issue. Make sure to clearly explain:

* The problem your idea solves.
* The user benefit.
* Any examples or comparisons (if applicable).

### 🛠️ Submit Code

To contribute code:

1. Fork the repository.
2. Create a new branch: `git checkout -b my-feature-name`
3. Make your changes (preferably in R/Shiny or HTML/CSS/JS as needed).
4. Add tests or validation steps where applicable.
5. Commit your changes and push the branch.
6. Open a pull request (PR) with a clear description of your changes.

**Note**: If you're unsure how your changes fit into the existing architecture, please open an issue first to discuss before submitting a PR.

## Code Style and Standards

* Use clear and descriptive variable names.
* Follow [Shiny best practices](https://shiny.posit.co/r/articles/improve/code-organization/) for separating UI and server logic.
* Comment your code where logic may not be immediately obvious.
* Avoid hardcoding labels

## Testing

Before submitting a PR, please verify that:

* The app runs locally via `shiny::runApp()`.
* File uploads and metadata generation work as expected.
* The download feature outputs a valid `.csv` file with clean formatting.
* The interface displays correctly on standard desktop and small screens.

## Communication

If you're planning a major feature or refactoring effort, please open an issue to discuss it with the maintainers first. This avoids duplication of work and ensures compatibility with future plans.

## License

By contributing, you agree that your contributions will be licensed under the repository’s [MIT License](LICENSE).

---

