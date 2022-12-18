<!-- ABOUT THE PROJECT -->
## github-ls

A simple tool that uses the GitHub API to list your repositories.


### Run

```shell
nix run github:damianfral/github-ls
```

#### Options

```
github-ls - List your github repositories

Usage: github-ls [--org ORGANIZATION] [--access ACCESS] [--display DISPLAY] 
                 [--lang LANGUAGE]

Available options:
  -h,--help                Show this help text
  --org ORGANIZATION       Print only repos owned by this organization
  --access ACCESS          Print only repos with this access (public|private)
  --display DISPLAY        Print field (name|url|ssh|git)
  --lang LANGUAGE          Print only repos matching this language
```
