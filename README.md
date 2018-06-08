# lajter - Flat query UI

The intent of this library is to address some problems creating a fullstack single page application with om.next. For me, it was hard to understand how to use all the pieces of om.next to get stuff done.

I think om.next has great ideas and I want to make it easier to use them.

Problems I had building an application with om.next - in no particular order:
* Figuring out how to compose component's queries.
* Figuring out why my component wasn't re-rendering.
* Figuring out how to get routing to work.
* Figuring out how to do optimistic updates.
* Figuring out which features to stay away from.
* Figuring out how to provide feedback to users with how their action went.
* Realizing why I didn't get all the data I needed from the server.

We eventually solved all these problems for ourselves, but it wasn't easy. Before I explain more about these problems (and how I've solved them in lajter) here's an overview of the differences between lajter and om.next:
* Left out:
  * QueryParams
  * Sub queries
  * Dynamic queries
  * App state normalization/denormalization
  * Component factories
  * Having to implement custom joins and unions to nest component's queries.
* New:
  * Flattened queries
  * Routing support
  * Optimistic mutation rollback (Layers)
  * Remote queries with contexts
  * Remote query resolver (mostly useful for datalog queries)
  * Remote response handling
* Kept in:
  * Structure: Reconciler, Parser, Indexer, Send, Merge, Shared
  * Server side rendering (wip)

I personally use datascript, which makes it easy to merge data into the app-state. If you'd want to use om.next's db-format, please let me know so we can make sure there are enough hooks to plug those pieces in.

Here are my thoughts on the problems I faced with om.next and how lajter solves them.

## Composing component's queries - Flat queries

I'd say the largest implementation difference between om.next and lajter is that queries flatter. Each (for lack of a better word) query-root is in the first level of a query. Components do not nest their children's queries.

Example query om.next:
```
TODO: Examples with code.
```

Example lajter:
```
TODO: Examples with code.
```

To get this to work properly, we need to merge the pull-patterns for each of the query-roots.

To mention:
* You have to come up with a way to do joins in om.next
* In lajter you declare the children you want to render.
* For routing you have to come up with a way to do unions in om.next.
* In lajter you declare the routing's a component has, and then you can render any one of those routes.

The way this simplifies implementation is that it removes the need for keeping track of which part of the parsed state belongs to which component. It also makes it easier to figure out what needs to get sent to the server. The indexer is easier to implement too. It can look at the first level keys of a component's query, compare it to the last time the app-state was parsed, and if it has changed, update the component.

## Routing

While it was possible to get the same routing I've implemented in lajter with om.next, it was not obvious how to do it. You had to.

In lajter you..

## Optimistic mutation rollback

This is something everyone might not like :)

## Remote queries with context

Queries have context on the client. They should be executed in the same context on the server.

## Remote response handling

In traditional UI development, where each component is responsible for requesting data from an endpoint, you have very granular control of how to handle the request and possibly view it to the user. This is all lost when transitioning to om.next. You have to make it up as you go along.

Lajter provides a way to access return values from the remote mutations, where you can place user facing messages or any other data that makes it possible for the UI to display feedback to the user.

Sometimes it's also necessary to show loading spinners whenever a request is in flight and one is waiting for the user. With om.next, there's no way to tell whether a read returned no data because there simply is no data, or the read returned no data because the request hasn't received a response yet.

With lajter, you have a log of all your requests and you can quickly check if a read has been sent and whether it has gotten a response yet.

TODO: Code examples.

## Remote query resolver

By using the lajt parser (petterik/lajt) it can make sure that the reads can easily add more data to the pull-pattern than it already has.

### More about the lajt parser

The lajt parser is an extensible parser that ...
