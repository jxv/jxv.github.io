# Fluid to use PureScript

As of last night, I decided to port [Fluid](https://www.fluid-idl.org)'s code generator from JavaScript to [PureScript](http://www.purescript.org).
Yay!
But it's working fine in JavaScript.
Why switch?
What happened?

## Code generator constraints

While Fluid has noblier ambitions by targeting support of 40+ languages, the generator can only be written in one.
So I can afford to be picky about which one.
In fact, I should be.
It's the glue that holds everything together.
It needs to be strong.

### Target language representation

Early in Fluid's prototyping phase, I flipped between using Haskell and JavaScript a few times.
Haskell excels at representing data.
JavaScript doesn't compare.
That makes sense because Haskell is commonly used to write compilers and other language ventures.
And I'll admit that's not what attracted me to the language because I'm not a PLT enthusiast.
I don't eagerly reach for or keep up with the latest language features.
I just wanted to write good code.
Simple, boring, and approachable.
That's enough for me.
And despite dragging my heels and feeling a few years behind, writing Haskell has been a pleasurable detour.
I'd [recommend it](https://github.com/bitemyapp/learnhaskell#how-to-learn-haskell) for its own sake.

#### PureScript vs Haskell

Because PureScript is deeply inspired by Haskell, it has many of the same core features.
And with this project, PureScript's goals happens to be more aligned than with Haskell's.
I'm not dropping Haskell.
However, it's been a refreshing change not dealing with historical quirks.

### Contribution and language barrier

40+ languages is _a lot_.
I can't confidently say I could learn all those including ecosystems within a reasonable time.
Keeping the barrier low for contributors is then the obvious choice.
JavaScript, being the modern "lingua franca" of software, has the edge by a wide margin compared to every language.

PureScript is niche, being even smaller than Haskell's community.
Warranted or not, Haskell has been notoriously difficult to learn.
While the barriers are lowering with Haskell and PureScript, I don't expect anyone using another language to jump in.

At the same time, this could be moot.
Most OSS projects don't have many contributors.

### Can support a web app

I'd like to have a playground web app.
One which generates a test client on the fly.

Both JavaScript and PureScript are created for the web.
Generally, JavaScript has an advantage because its vast and different amount of existing libraries.
But on the otherhand, Fluid just reads JSON and generates strings.
It's simple and doesn't really _need_ anything past that.

### IO

Minimally, Fluid reads the JSON spec, creates a directory, and writes files.
While I like PureScript's Effectful system with Monads because of how powerful they can be, the IO needs are basic.
And both work fine—for now.

### Distribute as a binary

Both can be distributed, but it's not that simple.
Writing "Isomorphic" JavaScript has been more difficult than I have hoped.
It needs to be transpiled with roundabout programs.

A major design goal of PureScript is transpilation.
Although originally intended to target just JavaScript, there are already other backend targets working in development.
For example, C++ and Erlang.
This makes me more excited for PureScript.
Like Lux, it's positioned itself as a language to be transpiled from.
It'd be interesting where that leads.

### Maintainability

All code will eventually be deleted, forgotten, or legacy code.
One day, I hope Fluid will become honorable legacy code.

As a personal principle, I prefer static type systems languages for projects that are going to stick around.
I'm dumb and forget things easily.
Along with functional purity, Haskell had given me a solid crutch to lean on.
PureScript will do the same.
Athough writing Haskell felt unnecessarily constrained at times, it's normally been worth it.
I can understand my years old code and another person's old code (more important) very quickly.
I don't feel anywhere near as anxious when making large refactors or adding incremental features.
_I don't feel same with JavaScript._
Definitely not feeling it right now.

## Conclusion

Overall, PureScript feels as though it's been on the upswing.
I'm happy to jump in and create.
