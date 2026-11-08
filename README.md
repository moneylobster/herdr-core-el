# herdr-core.el

An Emacs client for [herdr-core](https://github.com/herdrdev/herdr-core). Needs ghostel or vterm installed.

Caveat emptor: vibecoded, but works fine for me.

## Config

```elisp
(use-package herdr-core
  :vc (:url "https://github.com/moneylobster/herdr-core-el"
       :rev :newest))
```

## Usage

Call `herdr-core-dashboard` to see all the agents and do operations on them.

Use `herdr-core-spawn-agent` to spawn an agent at the current directory. This also works over TRAMP, provided you also have `herdr-core` running there and added it using `hc machine add`.

There is some other miscellaneous functionality, for example: `herdr-core-context-add` functions like `gptel-add` and lets you add the current buffer/file/region as context to your agent.
