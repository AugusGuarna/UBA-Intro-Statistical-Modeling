# Custom Wilcoxon Signed-Rank Test Implementation

This repository features a comprehensive statistical project developed for the **B.S. in Data Science at the University of Buenos Aires (UBA)**. The project bridges theoretical mathematical proofs with a custom computational implementation in R.

### 🚀 Key Technical Features
* **Object-Oriented Programming (OOP):** Built a custom `htest` class from scratch using R’s S3 and S4 systems to handle statistical outputs.
* **Statistical Power Analysis:** Conducted **Bootstrap estimations** (m=10,000) to evaluate and compare the power of the Wilcoxon test against the Sign Test and the UMP (Uniformly Most Powerful) Normal Test.
* **Mathematical Proofs:** * Proved the symmetry of the distribution for paired samples.
    * Proved the asymptotic normality of the test statistic using **Lindeberg’s Central Limit Theorem**.
* **Simulations:** Visualized the convergence of exact probability distributions toward asymptotic densities for various sample sizes (n=4, 10, 20).

### 🛠️ Technical Stack
- **Language:** R (Custom S3/S4 Methods).
- **Documentation:** LaTeX for professional technical reporting.
- **Concepts:** Non-parametric statistics, Hypothesis testing, and Asymptotic theory.

### 📂 Repository Structure
- `wilcoxon_implementation.R`: Core script containing the OOP logic, custom Wilcoxon function, and simulation loops.
- `statistical_report.pdf`: A 22-page technical report detailing the methodology, formal proofs, and performance results.

### 📈 Major Findings
Our analysis confirmed that while the Normal Test is the UMP, the **Wilcoxon test maintains high efficiency** even when parametric assumptions are relaxed, significantly outperforming the Sign Test in terms of statistical power.
