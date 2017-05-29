# Identify profitable currency exhange cycles

Requests current currency exchage table and identifies profitable sequence of exchanges ( arbitrage
detection ).

Example:

Given we have such rates:

```
Israeli New Shekel to Hungarian Forint : 76.835
Hungarian Forint to Israeli New Shekel : 0.013015
```

So - if we convert 1,000,000 `Israeli New Shekels (ILS)` then we receive 76,835,000
`Hungarian Forints (HUF)`. And if we make reverse conversion and exchange 76,835,000
`Hungarian Forints` then we will have approximately 1,000,007.52 `Istraeli New Shekels`.
We will have aboutn 7 shekels in profit :)

Such sequence of exchanges is shown in the application results as:

```
ILS <- HUF <- ILS
```

# Prerequisites

* GHCI ( v >= 7.10 )
* Cabal installed

# Installation

```
git clone git@github.com:vprokopchuk256/currency.git
cd currency
cabal update
```

# How to run

```
cabal run
```

# How It Works

Interanlly application receives current currencies table from [Fixer.IO](http://fixer.io/) and represents
it as a directed weighed graph. So the goal is to detect negative cycles in that graph.

Idea of the algorithm is taken from [here](https://www.youtube.com/watch?v=HoGSiB7tSeI&list=PLxc4gS-_A5VDvP_9W8JJ04zk6m1qTolzG&index=23).

## Contributing

* Check out the latest master to make sure the feature hasn't been implemented or the bug hasn't been fixed yet
* Check out the issue tracker to make sure someone already hasn't requested it and/or contributed it
* Fork the project
* Start a feature/bugfix branch
* Commit and push until you are happy with your contribution
* Make sure to add tests for it. This is important so I don't break it in a future version unintentionally.
* Please try not to mess with the Rakefile, version, or history. If you want to have your own version, or is otherwise necessary, that is fine, but please isolate to its own commit so I can cherry-pick around it.
