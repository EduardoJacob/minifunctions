
# if exists ".Rbuildignore", then add current file to it
if (file.exists(".Rbuildignore")) {
  current_file = basename( rstudioapi::getSourceEditorContext()$path )
  usethis::use_build_ignore(current_file)
  usethis::use_build_ignore("images")
  usethis::use_build_ignore("powershell")
  usethis::use_build_ignore("Claude.md")
}

# No caso de falhar o "Package Check" com Codoc mismatches from Rd file
# devtools::document()

usethis::use_git()

usethis::use_github()

usethis::use_readme_rmd()

usethis::use_mit_license()

usethis::use_roxygen_md()

# devtools::check()

# if removing a function or for any other reason there's an error in the documentation, you can use:
devtools::document()

usethis::browse_github()


# Start LM Studio if needed
terminal_id = rstudiotools::terminal("./powershell/LMstudioStart.ps1")
# Start Claude Code if needed
rstudiotools::terminal("claude --model qwen/qwen3.5-9b", terminal_id = terminal_id)

# Use Claude Code /init in order to describe the project and get a summary of the files in the project,
# which can be used to fill in README.Rmd

# Start LM Studio if needed
terminal_id = rstudiotools::terminal("./powershell/LMstudioEnd.ps1")















