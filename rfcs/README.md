# Typed Racket RFCs

Many changes, including bug fixes and documentation improvements can be
implemented and reviewed via the normal GitHub pull request workflow.

Some changes though are "substantial" and should go through an RFC process to
produce consensus within the (Typed) Racket team and community before
substantial work begins.

## When you need to follow this process

You need to follow this process if you intend to make "substantial" changes to
Typed Racket. What constitutes a "substantial" change may include the following:

  - Any semantic or syntactic change to the language that is not a bugfix.
  
  - Any changes that have non-trivial interaction with other portions of the
    language.

Some changes do not require an RFC:

  - Rephrasing, reorganizing, refactoring, or otherwise "changing shape does
    not change meaning".
  - Additions that strictly improve objective, numerical quality criteria

## What the process is

In short, to get a major feature added to Typed Racket, one must first get the
RFC merged into the RFC directory as a markdown file. At that point the RFC is
"active" and may be implemented with the goal of eventual inclusion into Typed
Racket.

Here are the general steps for an RFC feature/change:

  - Fork the Typed Racket repository
  
  - Copy `rfcs/0000-template.md` to `rfcs/text/0000-my-feature.md` (where
    "my-feature" is descriptive. don't assign an RFC number yet).
    
  - Fill in the RFC. Put care into the details: RFCs that do not present
    clear motivation, demonstrate understanding of the impact of the
    design, or are disingenuous about the drawbacks or alternatives tend to be
    poorly-received.
    
  - Submit a pull request for the RFC markdown document. As a pull request the
    RFC will receive design feedback from the larger community, and the author
    should be prepared to revise it in response.
    
  - Build consensus and integrate feedback.

## The RFC life-cycle

Once an RFC PR is merged it becomes "active" and authors may implement it and
submit the feature as a pull request to the Typed Racket repository. Being
"active" is not a rubber stamp, however it does mean that in principle all the
major stakeholders have agreed to the feature and are at least amenable to
merging it.

Reasonable modifications to "active" RFCs can be done in follow-up pull requests
to the RFC when necessary. Significant changes should be new RFCs, with a note
added to the original RFC.


## Why do I have to go through this?

Although the Typed Racket development community is relatively small, it is still
extremely useful to have a clear place to openly discuss and describe
significant new features to reduce wasted time and create descriptions which
future maintainers and contributors can reference.
