# How do I read lines from stdin and turn them into numbers?

The `string` module has a `to_int/2` pred.

# How do I write and run tests?

Include some predicates in your `main`. If they fail, the test failed.

There are a few test frameworks around, but for our purposes, they're not that useful here.

# What is "stylish" Mercury?

See the [Mercury Coding Standard](https://www.mercurylang.org/development/developers/coding_standards.html).

# How do I browse the available predicates and get help on them?

[`mmcdoc`](https://github.com/jrfondren/mmc-doc), which you can install using [`mmcget`](https://mercury-in.space/packages/index.html) together with `mmc --make mmcdoc`, might be some help.

# Is there an autoformatter?

The `tools/stdlines` script from the compiler repo will standardize the lengths of dashed section separators `%--…--%`.
