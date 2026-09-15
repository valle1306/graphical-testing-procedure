\name{run_app}
\alias{run_app}
\title{Launch the graphMTP application}
\usage{run_app(...)}
\arguments{\item{...}{Arguments passed to \code{shiny::runApp}, including
\code{port}, \code{host}, and \code{launch.browser}.}}
\description{Starts an interactive interface for graph construction, planned
one-sided group-sequential testing, and JSON session persistence.}
\details{Create hypotheses and transition weights on the Design tab. The
Group Sequential Design tab specifies planned information fractions and
spending rules. After finalisation, submit observed information counts and
one-sided p-values on the Analysis tab. Fractions map to rounded whole-number
information counts; observed information must equal these prespecified counts,
including at the final look. Sequential family alpha is at most 0.3173105.
Positive local levels must be at least 1e-5 and fixed first-crossing spending
increments at least 1e-8. Preview and execution use the same complete local
boundary family. Graph updates can enlarge nominal cutoffs during a batch;
the allocation and cutoff used for each event remain frozen in history.
Users supply valid endpoint statistics and justify their canonical joint-normal
information model. Information adaptations are unsupported. The HP-derived
spending shape is fixed at the initial positive allocation and scales with
recycled alpha; its nominal interim cutoff can consequently change.
JSON format 3 records execution semantics. Older completed histories import
as designs only, with a warning; new completed histories are verified by replay.
See the bundled README and replication materials for the supported scope.}
\value{The value returned by \code{shiny::runApp}, invisibly.}
\examples{\dontrun{run_app()}}
\seealso{\code{shiny::runApp}}
