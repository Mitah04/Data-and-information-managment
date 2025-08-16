# Conjunctive Query Engine

This project is a Scala-based engine for processing Conjunctive Queries (CQs). It provides functionalities to test for **acyclicity**, **containment**, and **minimality** of CQs.

## Project Structure

The project follows a standard sbt structure:

```
.
├── data/                  # Input query files (e.g., query-1.txt)
├── results/
│   ├── csv-results/       # CSV output files
│   └── txt-results/       # Detailed .txt log files for each test
├── src/
│   ├── main/scala/        # Main Scala source code
│   └── test/scala/        # Test source code (scalatest)
└── build.sbt              # sbt build configuration
```

-----

## 🚀 How to Run the Project

The main application is designed to run automatically on all queries present in the `data/` directory.

### Prerequisites

  * Java Development Kit (JDK)
  * sbt (Scala Build Tool)

### Execution Steps

1.  Navigate to the project's root directory in your terminal.
2.  Run the main application using the following sbt command:
    ```bash
    sbt run
    ```
3.  The application will automatically:
      * Parse all `query-*.txt` files from the `data/` directory.
      * Run acyclicity and minimality tests on each query.
      * Run containment tests for every pair of queries.
      * Generate `main-output.csv` and `containment-output.csv` in the `results/csv-results/` directory.
      * Generate detailed log files (e.g., `test-acyclicity-1.txt`) for each test in the `results/txt-results/` directory.

-----

## 🧪 How to Test New Queries

Testing new queries is simple:

1.  **Create a new text file** in the `data/` directory.
2.  **Name the file** following the convention `query-<id>.txt`, where `<id>` is a unique integer identifier for your new query. For example: `query-11.txt`.
3.  **Write your conjunctive query** as a single line inside the file. For example:
    `Answer(x, z) :- R(x, y), S(y, z).`
4.  **Run the project** as described above (`sbt run`). The engine will automatically include your new query in all tests.

-----

## How to Test Specific Features

While the main application runs all tests, you can analyze the output to understand the results for specific features.

  * **Acyclicity**: To check if a query (e.g., `query-5.txt`) is acyclic, run the project and then open the corresponding log file `results/txt-results/test-acyclicity-5.txt`. This file will show the step-by-step execution of the GYO algorithm. The final result (1 for acyclic, 0 for cyclic) will also be in `main-output.csv`.

  * **Minimality**: To check if a query is minimal, check the log file `results/txt-results/test-minimality-5.txt`. If the query is not minimal, this file will detail which atoms were removed. The final result is also in `main-output.csv`.

  * **Containment**: To test if `q1` is contained in `q2`, run the project and inspect the log file `results/txt-results/test-containment-1-2.txt`. The file will either show a valid homomorphism (if contained) or a counterexample database (if not contained). The summary result is in `containment-output.csv`.
