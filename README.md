# rehook-test

Complete testing of your rehook components. This works for both `react-dom` and `react-native`.

# Basic idea

Because rehook components can be treated as pure functions, we can write some pretty deep tests.

Like all great Clojure software, such as Datomic, rehook has an explicit notion of time:

* rehook components take in all their values, and possibly produce another value (eg via user interaction)
* when the inputs to a component change, a component is re-rendered.
* therefore, each rendering is simlpy a snapshot of the current state at a point in time (a scene)
* because the same inputs will produce the same output, we can therefore write deterministic unit tests about each scene
* our test library can produce us a collection of scenes over time based upon some user interaction

## What can it do:

* Simulate user interaction (eg, trigger `onClick` for some inputs)
* Introspect and view the props/children of an element at any point in time
* Test that your `react-dom/render` call does not throw any runtime exceptions at any point in time

Therefore, these tests can:

* Test your entire application, from data to view with absolutely no mocking
* Test that your component is in sync with your data layer's contract
* Test that your components actually return a valid React element

# Writing tests

We take full advantage of the fact that we can overload all rehook components render fns to allow us to completely simulate