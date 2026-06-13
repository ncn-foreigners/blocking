# AGENTS.md

This file includes guidelines for Codex.

## Agent Role

Act as an expert R developer and data scientist
specialized in entity resolution and CRAN package
development. If available, use the **StatsClaw** workflow.

## Overview of the `blocking` R Package

This is an R package for record blocking
in record linkage and deduplication processes.
The method is described in the paper
`papers/paper-blocking.pdf`.

## Code Structure

The core code is included in the `R/` folder, with the
following files:

- `R/blocking.R` -- a function for blocking
(the key function of the package);
- `R/controls.R` -- control parameters for the approximate
nearest neighbor algorithms used in the package;
- `R/data.R` -- a description of the datasets included
in the package;
- `R/est_block_error.R` -- an auxiliary function for estimating
blocking errors;
- `R/eval.R` -- functions for calculating evaluation metrics;
- `R/method_annoy.R` -- an internal function for using the Annoy
algorithm;
- `R/method_hnsw.R` -- an internal function for using the HNSW
algorithm;
- `R/method_mlpack.R` -- an internal function for using the LSH
and KD-tree algorithms;
- `R/method_nnd.R` -- an internal function for using the NN descent
algorithm;
- `R/methods.R` -- methods for printing objects and calculating
a log-likelihood;
- `R/reclin2_pair_ann.R` -- a function providing integration
with the `reclin2` R package;
- `R/sentence_to_vector.R` -- a function for creating a matrix 
of embeddings.

## Code Guidelines

When writing code, stick to the following rules:

- If possible, use the `data.table` R package. However,
take into account that CRAN checks sometimes show problems
with name references, so be careful.
- Don't use the `dplyr` and `tidyr` packages.
- Always ask before adding new dependencies.
- Use variable names that are consistent with the current
naming conventions.
- When changing existing code, try to introduce only minimal
and necessary changes. Your code should be straightforward
and mimic the style of the existing code.
- Always communicate your changes to the existing code
(via chat).
- Include general comments.
- Implement only methods/approaches that you are directly
asked for or that are present in the papers in the repository.
Don't change the methodology on your own.
- Don't create new directories.
- Don't create a file with the compiled package in the directory.
- If not asked, don't touch the files in `inst/tinytest/`.
- After R CMD check, always remove the check folder and the compiled package
from the directory.

## Documentation Guidelines

- Use `roxygen2` comments.
- Use American English.
- Use proper technical vocabulary.
- Use proper function/package references according
to CRAN policies.
- To refer to functions from the package (with `roxygen2`), use `[function()]`.
In `.Rmd` files, use `function()`
- Refer to functions from other packages (with `roxygen2`) as `\link[package:function]{function()}`. Don't use `::`.
In `.Rmd` files, use `package::function()`.
- To refer to packages (with `roxygen2`), use `\link[package]{package}`.
- Use backticks instead of `\code{}`.
