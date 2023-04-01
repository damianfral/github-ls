# github-ls

A simple tool that uses the GitHub API to list your repositories.

## Run

```shell
nix run github:damianfral/github-ls
```

### Options

```text
github-ls - List your github repositories

Usage: github-ls [--org TEXT] [--access ACCESS] [--display DISPLAY] 
                 [--lang TEXT] [--archived] [--sort SORTING]

Available options:
  -h,--help                Show this help text
  --org TEXT               Print only repos owned by this organization
  --access ACCESS          Print only repos with this access (public|private)
  --display DISPLAY        Print field (name|url|ssh|git)
  --lang TEXT              Print only repos matching this language
  --archived               Print also archived repos
  --sort SORTING           Sort by (name|update|age)
```
