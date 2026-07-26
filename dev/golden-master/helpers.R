# Helpers for comparing the in-development package's behavior against a
# specific git tag ("golden master" / characterization testing).
#
# NOT part of the installed package (see .Rbuildignore): this depends on the
# package's own git history, which a built/installed package does not carry,
# so it must never be shipped or run as part of R CMD check on a tarball.

#' Path to the root of the git repository containing this script
#'
#' Requires the current working directory to be somewhere inside the repo
#' (any subdirectory works - git walks up to find it).
git_repo_root = function(){
  out = suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = TRUE))
  status = attr(out, "status")
  if (!is.null(status) && status != 0){
    stop("Not inside a git repository (or git not available):\n", paste(out, collapse = "\n"))
  }
  out
}

#' Read a file's content as it existed at a given git tag/ref
#'
#' Does not check anything out - reads the blob directly via `git show`.
#'
#' @param tag Character. A git tag, branch, or commit-ish ref.
#' @param path Character. Path to the file, relative to the repo root.
#' @param repo_root Character. Repo root path (default: auto-detected).
#' @return Character vector, one element per line of the file.
read_file_at_tag = function(tag, path, repo_root = git_repo_root()){
  out = suppressWarnings(system2("git", c("-C", repo_root, "show", paste0(tag, ":", path)),
                                  stdout = TRUE, stderr = TRUE))
  status = attr(out, "status")
  if (!is.null(status) && status != 0){
    stop("Could not read '", path, "' at tag '", tag, "'. git output:\n", paste(out, collapse = "\n"))
  }
  out
}

#' Source one or more files, as they existed at a given tag, into one fresh environment
#'
#' All `paths` are sourced into the SAME environment, so functions that call
#' each other by bare name (e.g. the old adjscores_A2024() calling
#' formula_transf_text()) resolve correctly, exactly as they did when the
#' package was a folder of loose scripts sourced together.
#'
#' @param tag Character. A git tag, branch, or commit-ish ref.
#' @param paths Character vector of file paths, relative to the repo root.
#' @param repo_root Character. Repo root path (default: auto-detected).
#' @return The environment the files were sourced into.
build_old_env = function(tag, paths, repo_root = git_repo_root()){
  env = new.env(parent = globalenv())
  for (path in paths){
    code = read_file_at_tag(tag, path, repo_root)
    source(textConnection(code), local = env)
  }
  env
}
