# lajter - Flat query UI

The intent of this library is to address some problems creating a fullstack single page application with om.next. For me, it was hard to understand how to use all the pieces of om.next to get stuff done.

I think om.next has great ideas and I want to make it easier to use them.

Problems I had:
* Figuring out how to compose component's queries.
* Figuring out why my component wasn't re-rendering.
* Figuring out how to get routing to work.
* Figuring out what to do about optimistic updates.
* Figuring out which features to stay away from.
* Realizing why I didn't get all the data I needed from the server.

Before I explain these problems and how I've solved them in lajter, here's an overview of the differences between lajter and om.next:
* Kept in:
** Structure: Reconciler, Parser, Indexer, Send, Merge
** Server side rendering (wip)
** Shared (wip)
* Left out:
** QueryParams
** Sub queries
** Dynamic queries
** App state normalization/denormalization
** Component factories
** Having to implement custom joins and unions to nest component's queries.
* New:
** Flattened queries
** Routing support
** Optimistic mutation rollback (Layers)
** Remote queries with contexts
** Remote query resolver (mostly useful for datalog queries)

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

## Remote query resolver

By using the lajt parser (petterik/lajt) it can make sure that the reads can easily add more data to the pull-pattern than it already has.

### More about the lajt parser

The lajt parser is an extensible parser that ...
