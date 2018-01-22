## Links:

* jthomsom om.next notes: https://gist.github.com/jpthomson/6f0333defcaa9b1b29f914bb965c99ef

## Notes from a plane between Montreal and San Francisco

What are the essentials?

Read, mutate, parser?, send, merge, render, state?, response, routes, ?

What can we learn from Posh, Fulcro?


## Excluded from om.next
QueryParams doesn't work.
- We can have dynamic queries anyway, since the reads can be ultra powerful.

Dynamic queries doesn't work.
- i.e. set-query, update-query
- Which means that we can simplify the indexing logic a lot?

Don't need merge-tree, tree->db, db->tree.


## Reads

Read results should be cachable, to return reads instantly.
- Doesn't take up any extra space thanks to immutability.
- Reads can be cached in om.next, but a pass of path-meta is needed, which destroys (free) pointer equality checks.

Changes to data should reactively update UI
- om.next doesn't have this.
  - It uses an indexer and queued components to trigger re-renders (I think).
- Knowing the underlying database format (e.g. datascript) would make it easier to implement reactiveness.
  - It's possible that we can create protocols for this stuff though.
- Equality checks can be run understand if data needs updating and UI needs rendering.

Reads should (must?) declare their entity-query.
- Where entity-query returns entity ids that can be pulled from.
- Reads without an entity-query cannot have a pattern?
  - Then we can fall back on making them work as they do in re-frame.
  - There are cases where one doesn't want to specify queries, but we'll have to come up with examples.

Can we make counts or similar queries fast?
- Counts can be done instead of pull. That's interesting.
  - Meaning, let's say reads declare an entity-query:
    - The read can then decide whether to pull on those entities or do whatever they want, like a count for example.
- Could we make it possible to depend on other read's queries?
  - This would be nice.
  - There's one read declared with entity-query + pull pattern. Another read depends on that reads' entity-query.
    - Whenever the result for the entity-query changes, the reads depending on that read would also be run.
    - Do we want to split out entity-query, post-fn (e.g pull or count) all together.
      - Making them interceptors like in reagent?
- Example
  - :query/notifications
  - :query/notification-count
  - They both specify the same entity-query but one pulls and the other counts.
- Well, ok, how about totally custom reads.
  - For the ones that don't rely on app-state data
    - They can't have a pull-pattern
    - They will be re-run everytime the component changes.
    - They will be re-run whenever we merge the key
    - They will be re-run whenever an action explicitly says they should be re-run.
  - For the ones that does very custom logic and has a pull pattern
    - They can depend on 1 or many other reads for their query data.
    - They can use the post-read function (for sort and stuff) to do whatever they want.
    - This way we've specified which data they depend on.
      - I think this is sick.
      - We can here combine custom queries and entity-queries, that eventually is used in another query.
        - entity-queries should have default optimizations, such as has anything changed. "More on this later".

Seems like we want a post-query hook for reads?
- This is possibly just an intersector
- So that the user can sort and stuff after the query/pull is done.
  - This would be the value we cache.
    - this is also the value used by the components, which means it's basically free to cache.

Should be easier to declare whether a read should be remote or not?
- No: (if target {:remote true} ..)

## Mutates
Works pretty well?

Specify reads that should be forcibly re-run?
- Like the example where there's no query and no pattern.

## Parsing - Will we still have it?
What's nice about parsing, is that it's easy to run and extend a parser.
Defining "global rules" works pretty well UI programming (like reagent/reframe does?) but it's super nice to
just have functions instead that you can call or not. Not a fan of globally defined stuff. And whenver something
doesn't feel just right, it's probably because it's stateful, which global things are.

The parser is really the engine for reads, mutate and somewhat sends. It can change the outcome of all of them and.
It's nice to have such functionality so easily extended.

Parsing should dedupe query.

## Send
Should be pluggable, but default to git-rebase.
One shouldn't have to define an action and an inverse-action. It's 2018.

## Merge

Multimethod for merge has been great for us. 
- Rarely used but gives a lot of power.

Should possibly also always call a function whenever a key or mutate is merged.
- To be able to do "post-merge" stuff.

## Render
Defining UI components must be easier to do.
Having a map with life-cycle methods is a must.
- fulcro's defsc is nice.
Defining queries could be nicer?
- Always specifying children component joins and their query is repetative and could be nicer?
- Also possibly defining which children one has in a vector: [Product Login]
  - Possibly associated with a key: [[:product Product] Login]
    - If one needs multiple or something?

## Component state
Do we care?
What does reagent do?
I think they use reactive atoms, which are sharable. Pretty cool?
What om.next does is fine too.
- Just hooking in with react or with a protocol called ILocalState (or something).

## Response

I really like the way we implemented messages. 
Something similar usage wise would be nice.
Implementation could be different?

Read responses as well?
- (I.e. listen for if we've gotten a read or not).
  - Has always been an anti-pattern.
- What do we want here?
- I think we want: 
  - "Have we read remotely ever?"
  - "Have I been unmounted have we read since?"
  - not sure.

## Routes
What can we do to help with routes?
- Registering routes on a multimethod allows for code splitting
  - Which is nice to have.

Make it really easy to depend on routes, route-params or query-params.

# What really makes this possible is the tech that can reactively update UI components, quickly, given a query and a pull-pattern.

Entity was changed
- Entity attribute was added
- Entity attribute was deleted
- Entity attribute was changed.
Entity was added
Entity was deleted

How many entities are there in our database?
- Like, what if we store each entity that was returned for a read.
- Check if any entity was changed

To update reactively.
- For each new tx-data,
  - If entity is new
    - Re-run reads that includes the attribute/entity.
      - Possibly do something smarter, where we can incrementally update the read data.
  - Lookup which reads the entity belongs to.
    - Possibly incrementally update read results based on new tx-data.
      - If the entity was in the pull-pattern:
        - We could store the path of the entity-map, and apply new tx-data to the map.
      - If the entity was relevant to the query:
        - If the entity could be one of the return values
          - Pass it as input
          - else: Maybe re-run the whole query?
  - We're doing a lot of indexing.
    - Could we index the data via Datascript and have our reactive logic be datalog?

Incrementally updating pull-pattern data would make equality checks faster, as we're not
- Creating entirely new structures for the data that's the same.
- Making should-component-update fast again.

Looking through our client/parser/read.cljc it seems like what's been described would work for us.
- We mostly use (db/pull-x-with db query eid(s)).
- And it's pretty cool that this could allow us to cache ids and pull separately.

## Server
What happens on the server?

Reads that depend on others will be run sequentially, others in parallel.

The whole engine (with reactive updates and stuff) can work on the server as well.
- Updating certain reads when there's new transactions in the tx-queue.

## Testing
I don't see any benefits to testing. Om.next is quite good as it is.

## Is this really just parts to om.next?
- Indexer
- Parser
  - Maybe just read/mutate, since we can elide path-meta.
- ?

## Indexing with different pull-patterns?
- For sulo, we just merge pull patterns, which would end up just being 1 index per read+pull-pattern.
  - If someone, for some reason, wants only the data their UI component asks for, we could add a {:strict true} flag.
    - This could strip the data from what the pull-pattern returns
    - OR
    - have separate indexes depending on pull pattern. This seems insane though.

## Implementation

### Reactive query

#### New datom

If new datom is in the find-pattern:
- Doesn't matter. If it matters, it'll be in the where clause.
  - And if it's an input, it'd have to be found by some other query and then passed to this one.
  - Meaning we only need to index the where clauses.

Find the attribute of the datom in the where clause
- Pass the datom's eid as input to the where-clause's symbol.
  - Place the where clause at the top to filter the query heavily by it.
    - (which probably means we want to place where-clauses above it, in reverse)?
- Execute the query
  - Optimization: Implement a version of the query that's (possibly) lazy and where the analysis of
                  the query is cached.
                  - Lazy because we might only need the first result.
  - If the query returns entities that we're not previously in the entity-set
    - Add these entities, execute the pull-pattern part.

### Reactive pull-pattern

#### New datom

For the attribute of the datom:
- Get all pull patterns that contain the datom's attr
  - Get the path in the pull pattern to the attr
  - Find a match between the entities returned by the read (cached?) to the new datom.
    - Walking with datoms, depth first, aborting when found, is probably faster than query.
      - Odin instead of self-made transducer?
  - Update the pull-pattern result with the new datom.
    - Update all attributes for the datom at once.

#### Datom change
Value change or deletion.

For pull-patterns that returned the datom
- Get paths
  - Update pull-pattern

#### Recursive pattern

Something to think about.

#### Optimizations

- Given that we need to find a single match across datoms
  - Can we sort the datom sets by count?
    - Should be (as->> (d/datoms db :eavt) $ ;; returns an datascript.btset.Iter
                       (- (.-right $) (.-left $)))
      - Might not be entirely accurate as the sets may contain empty positions?
      - Initial test with only 5 datoms show that the count is accurate with:
        (- (.-right $) (.-left $))
    - There's actually a function for count: datascript.btset/est-count
    - Doesn't actually need to be fully sorted, can just bubble up the smallest Iter
      - Like clojure.set/union and intersection.

#### Restrictions

Only support find patterns [?e .] and [[?e ...]]
- At first at least.
- This makes sense because a pull pattern is executed on an eid or eids.
  - tuples can be achieved by having dependent queries
  - and for entirely custom queries
    - wait. How do we make these queries reactive? I forget.
      - What if they specify a function that:
        - given the environment (app-state, last return and stuff)
        - check if you should be re-rendered.

Don't support query rules for now?
- They are recursive and it can get messy?
  - Just avoid for the 0.1.0 release.

#### It's just a parser

Reads should always return what has been read.
- when reads return exactly the same thing, the UI component won't be updated.
- the whole query should really be able to be run every re-render.
- it's not so much a reactive UI, it's just a fast full-re-render?
  - the props should just be taken from the path of the UI component in the query
- each reconciliation should provide the tx-queue (changes since last time)
  - and db when the last read was done.
    - what about params?
      - just store all db values for all param values?
It's about indexing reads
- their where clauses and pull patterns.
- it's about having reads depend on each other.
  - which is an implementation detail.
- it's about incrementally updating pull and query results.
- this is it?
  - it's portable.
  - any om.next user can use it.
- equality checks might be useful
  - can't think of a usecase though.

UI components doesn't need paths. The parser will be deduped with merged pull pattern.
- The components can just get the result from the root of the parsed map.

Params will be the same for all components as it's the route that decides them.

This is how we get rid of path-meta. UI->props will have a flat map (or datascript db with the indexed&cached reads).

Will have to make parser lazy for the union queries, to avoid re-parsing the query all the time.
- the query will be static? Never change?

Think about git-rebase. As one is jumping to a different db most of the db should be equal.
- Fast equality checks might help here.
- Will need to get the tx-report queue here as well.
- incremental updates break.

#### Implicit read dependency problem

When writing reads, they sometimes implicity depended on other reads
and one just hoped that we'd have that data on the front end.

What this really meant was there is a where clause that depend on some
data, this is explicit in code but the om.next parser doesn't handle this.

What we can do is - in our reads - declare query dependencies and get the
pull pattern from the where clauses of that query.
- First solution could specify the query pattern (of the query dependecy reads)
  manually instead of getting it from the where clause.

#### Patterns aren't supplied in the reads

We'll have to dynamically index pull patterns as they're not specified in the reads.
- We could move this specification to the reads, but I don't think that's as nice.
- I think as a component developer, you want to be able to specify what you need.
  - It also makes it easy to see what one can expect in the props.

Hopefully this doesn't become too much of a problem.