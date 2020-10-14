# Styling

Use `purty` and `editorconfig`.

# Module structure

Everything normally goes into `HyperWidgetConfig` module.

If you have something general that is not dependent on or related to
the domain logic/types, then it goes under top-level modules like `Data`.

If you have some functionality that is directly related to already existing
module but just not there for whatever reason, then go with their original module
name but prefixed with `Extra.`. For example, functionality that extends `Data.Lens.Prism`
goes as `Extra.Data.Lens.Prism`.

# Heterogeneous type-class constraints

Any function that uses `hmap` or `hfoldl` usually ends up with a _huge_ type-class constraint list
in front of it. So to make it more readable it makes sense to encapsulate such functions in its own
type-class and prefix its name with `h`, for example `hsequence` or `htoNullable`.

A side tip: when working with `hmap` and `hfoldl` it is often a good idea to annotate types as much as possible
to help compiler infer proper types. It has some problems with that as the types are so polymorphic.
Also it helps to apply functions directly without `$` as it can cause some `type variable escapes its scope` errors.
