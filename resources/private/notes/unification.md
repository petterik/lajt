Another frontend framework. Aff.

om.next and re-frame is almost the same thing.

By flattening om.next's query, removing the need for joins for child components, re-frame is basically the same thing, where reads are registered in a global atom.

It might be possible to write a frontend library where both re-frame and om.next style frontend dev is possible.

Why? What happens when we can realize that they are the same. Re-frame would get the same client<->server mental model, it could be adapted to the backend, fullstack frameworks galore? (what does that mean?)

This would mean putting lajt aside a little bit. Maybe just get merge of unions working and finish the work with dependencies.

Then writing a framework core, that:
- uses the lajt.parser, flattening queries and stuff.
- components have queries (just like in re-frame too!)
- components doesn't have to specify joins for children components, which you have to in om.next, but not re-frame.
- ui->props gets the parser's result for each "root key".
- remote queries is a thing.
- send is a thing.
- mutations are expressed like in re-frame? (instead of wrapping actions).
  - Which means there are at least 2 things. effects and event-effects.
    - Event effects return effects. This is because there are no click handlers?
  - Effects do some stateful thing, like resetting the db.

!!! Hold on. I don't understand re-frame yet.

Since all events in re-frame come from the same core.async channel, they work in slightly different ways.

What's better though?

I think I can't see the horizon of the stuff I think I'm getting my self into.

Cuz I'm in too deep... Re-frame's docs is having an effect on me.

Wait, I was wrong about effects. I was more of a special case kind of thing?

It's cool though that the parser could be dereffable? Returning what it's currently holding for any key.

lajt.read is not general enough to be lajt.read.
One could choose to define reads with interceptors instead for example. Wait, nvm. One could just "compile" down from om.next reads or re-frame style effects to lajt.reads.

Questions:
- What are the goals and why?
  - One goal could be to unify web dev tooling.
- Are there click handlers in React views?
  - Are they parsed when rendering?
  - Would pluggable render fix any issues here?
- Re-frame's data flow is very simple. Everything goes through the same channel. As where in om.next, you have click handlers, send function and .. that's it?
  - Is there a downside to splitting out the data flow into separate stuff?
  - Does it matter?
    - Merging app state could just be a mutation, instead of some weird callback.
- Can we model all the effects, event effects and co-effects as mutations?
  - What are all these things?
  - It seems like we could easily express a re-frame event like [:change-clock-color "red"] to a mutation [(change-clock-color {:args "red"})], then in the :mutate change the symbol to a keyword, and back again.
- What does subscriptions really mean?
  - How does re-frame know what to re-render?
- When using subscrpitions, I've seen them often used inside of render, is lifting them out of render such a bad thing?
-
