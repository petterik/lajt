# lajt

Declarative datascript UI with om.next like libraries.

## Rationale
Library that aims to make creating UI's with datascript with om.next like libraries easier, better and faster.

Library will feature a declarative parser.
The parser will make it possible to create a reactive-like UI, where whenever data changes in the datascript store, the UI components that need updating will be updated.

The declarative parser might solve some problems with remote data fetching, as using datascript causes different problems as the default db format om.next uses.

👷 Library is still in its initial design phase.

## Motivation
### A case for using DataScript with om.next like client libraries
#### Or: Client app state management with a client database
#### Or: Any sufficiently complicated client app state contains an ad hoc informally-specified bug-ridden slow implementation of half of a database.

Note: Whenever I mention om.next, I'm regarding to om.next with the default db format.

With om.next and Fulcro we're managing our state with maps that form a database like structure. It's normalized like a database and we can perform CRUD operations on it like database. Reads and writes to these data structures are either custom functions for each read & write, or they're guided by queries where the write data has to match the shape of the queries to be able to be written. For reads, it'll simply extract the root-nodes (or tables) of the queries from the database. This is the big win I'd say.

| App state operation| Datascript | om.next / Fulcro |
| -------------------|------------|------------------|
| Writes             | Database transaction data | tree->db, Takes a query and data with the shape of the query and merges it with the app-state. |
| Reads              | Datalog and Pull API | db->tree, Walks the app-state with the query to extract the app-state into the shape of the query. |

### Differences
- Low flexibility for server<->client communication, since server must always match the shape of the query. Which might not be a bad thing.
- Creating non-trivial custom queries over your app-state?
- Very easy to model a delete in the Datascript case, both for cardinality one and many. In Fulcro and om.next, I don't know?
- Very little "read" or "parser" code is needed. The data must match the shape of the query, but when it does, you just need to write your component queries and you're done. For Datascript, every "root node" of your query will need to be defined. The queries might also need more data than what you're components have defined.

Note: There are cases when you do need a parser in Fulcro, but I'm not experienced enough to know when this is.

By having a hard requirement on the shape of the data, om.next and Fulcro makes reading the app-state trivial and the client will always have the data it needs. Datascript clients has a more flexible requirement of the shape of the data, but needs to define each read from the app-state and it'll have to make sure that these reads have all the data they need to be fulfilled. Potentially requesting more data than the component's queries have defined.

It's not clear that the DataScript client is a better approach than the om.next and Fulcro  clients. Having written a fullstack application using DataScript I found it to be quite nice. One of the benefits is that the client acts very much like our Datomic backend. The reads one have to define is very similar (if not the same) as the server reads. So I wanted to explore how to make the creating a DataScript client easier and so far I've created a flexible parser and a library for defining reads declaratively.
### Goals

The goal with the library is to:
- Make DataScript clients easier to write. Reads, writes, merge, send, everything.
- Taking care of all the remote data issues.
- Re-rendering components reactively, essentially making re-render happen based on the data that's written to DataScript.
- A lot of caching to make reads incrementally updated as much as possible.
- Providing a clear path to how to use routes and route params.
- Making it easier to perform mutations, without having to know which reads need to be re-rendered.
- Keeping it small.
- Extendable, such that the library is defined with extendable primitives.
- Parser, read and mutate interceptors instead of middlewares to avoid deep call stacks.
- Clojure and ClojureScript library, so that you can use it for server side datomic queries as well as frontend queries. Which also makes it easy to use for SSR.
- Async parser for front- and backend.

Lajt will work with om.next and I'll integrate it with [sulolive(https://github.com/eponai/sulolive)] to use as a proof-of-concept in a non-trivial application. I would like it to work with qlkit and future om.next like libraries, but that's a later problem.

#### UX
I'm hoping the user/developer experience for this library will be to replace the om.next or qlkit parser and define your reads something like this:
```clj
;; Defining read :people/with-first-name and :selected/person
;; datascript pull or pull-many is called if the queries are used with a pull-pattern.

(def reads
  {:people/with-first-name
    {:query '{:find  [[?e ...]]
              :where [[?e :person/first-name _]]
   :selected/person
    {:query '{:find  [?e .]
              :where [[?e :person/id ?id]]}
     ;; Getting person from route data
     :params {'?id [:route-params :person-id]}
  ;; ...
  }})
```
`:query` and `:params` is defined in the `lajt.read.ops` namespace. The library will come with enough operations to describe the [sulolive(https://github.com/eponai/sulolive)] app's reads, but if you need to define your own, you can. The order in which the `ops` are run is defined in the `lajt.read.deps` namespace using stuartsierra's dependency library.
I feel like I'm getting into the implementation details now, so I'll stop here. If you want to see a bunch more reads, see the on-going sulolive integration which currently contains both its old reads and the lajt reads:
https://github.com/eponai/sulolive/blob/petter/lajt-integration/src/eponai/client/parser/read.cljc

If you think this is interesting work, please let me know (or star) to help motivate me.

## Usage

FIXME

## License

Copyright © 2018 Petter Eriksson

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
