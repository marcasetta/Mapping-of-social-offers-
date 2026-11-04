# Language (Technology) is Power

This repository contains a seminar presentation by Marta Caserio and Rebekka Schade, given on May 8, 2025, in the seminar *Data Feminism and Data Ethics*. The presentation discusses the paper *Language (Technology) is Power: A Critical Survey of "Bias" in NLP* by Blodgett, Barocas, Daumé III and Wallach (2020), and reads it alongside Catherine D'Ignazio and Lauren F. Klein's book *Data Feminism*.

## What the paper is about

The amount of research on "bias" in natural language processing has grown rapidly, but it is often unclear what researchers actually mean by the word. Blodgett et al. set out to examine this by surveying 146 papers that analyze bias in NLP systems. For each paper, they looked at two things: the motivation, meaning the harm the authors say they are concerned about, and the technique, meaning what they actually measure or try to mitigate.

Their central finding is that the field's understanding of bias is often vague and inconsistent. Many papers never explain why the system behavior they study is harmful, and the techniques they use frequently fail to match the harms they claim to address.

## How the papers were categorised

The authors grouped the papers by the kind of harm involved. *Allocational harms* occur when a system distributes resources or opportunities, such as credit or jobs, unfairly. *Representational harms* concern how groups are portrayed. These include stereotyping, as well as differences in system performance between groups, misrepresentation and denigration. A further group of papers looks at *questionable correlations* between system behavior and features associated with particular groups. Some papers fit no clear description at all, and a final group consists of surveys, frameworks and meta-analyses.

Comparing motivations with techniques shows where the gaps lie:

| Category | Motivation | Technique |
|---|---:|---:|
| Allocational harms | 30 | 4 |
| Stereotyping | 50 | 58 |
| Other representational harms | 52 | 43 |
| Questionable correlations | 47 | 42 |
| Vague / unstated | 23 | 0 |
| Surveys, frameworks, and meta-analyses | 20 | 20 |

## Key findings

The motivations behind the surveyed papers are wide-ranging, and many papers name several at once. Yet 23 papers, about 16%, give no clear motivation, and 47 papers, about 32%, offer no normative reasoning for why the behavior they describe is a problem. Even where a motivation is stated, it often remains unclear who is harmed, in what way and why. Papers also tend to blur the line between allocational and representational harms.

The techniques show similar weaknesses. Most draw little on research from outside NLP, such as sociolinguistics or the social sciences. The mismatch with motivations is striking: around 21% of papers cite allocational harms as a concern, but only four actually measure them. Most work focuses narrowly on system predictions and datasets, and because papers measure such different things, their results are hard to compare.

## What the authors recommend

Blodgett et al. argue that research on bias in NLP should be grounded in the literature on language and social hierarchies. Language is closely tied to power, and beliefs about which ways of speaking are "correct" often reinforce existing hierarchies. Researchers should therefore ask how social hierarchies, language ideologies and NLP systems shape one another, and treat representational harms as harmful in their own right, not only as precursors to allocational ones.

They also call for explicit conceptualizations. Researchers should state clearly which behaviors are harmful, in what ways, to whom and why, and be open about the normative reasoning and assumptions behind their work.

Finally, the authors emphasize engagement with affected communities. This means centering the lived experiences of the people affected by language technologies, questioning the power relations between technologists and those communities, and considering whether some systems should be built at all.

Taken together, the paper's message is that analyzing bias is always a normative process. Technical fixes alone cannot address inequalities that are rooted in society.

## Our reflections

If analyzing bias is inherently normative, then the definition of "bias" itself becomes a central question. This led us to reflect on our own vocabulary. Why have terms like *bias*, *fairness* and *racial bias* become established in data science, while affected communities have long used words like *discrimination*, *oppression* and *racism*? The choice of language can soften or obscure what is at stake.

We also noticed a tension in the paper's framing. It argues that algorithmic fairness techniques can preserve existing power relations by keeping decisions in the hands of technologists. This implicitly treats technologists and affected communities as separate groups, which overlooks the work of technologists who themselves belong to affected communities.

## Connection to *Data Feminism*

The paper connects closely to Chapter 2 of *Data Feminism*, "Collect, Analyze, Imagine, Teach." The common goal of "mitigating bias" suggests that an unbiased state is achievable, an idea that resembles the imagined objectivity the book criticizes. The paper's demand to analyze which harms occur, to whom and why mirrors the book's call to examine the historical and social roots of bias. The solutions both texts propose also align: drawing on knowledge from outside the field, acknowledging the normative nature of the work and involving affected communities. Both suggest that data science can be challenged from within, but that data science alone is not enough.

## Discussion questions

We closed the presentation with two questions for the seminar.

The first concerns the existence of automated systems. If we assume that such systems will always be biased, and that this bias will always cause serious harm to marginalized communities, should these systems continue to exist?

The second concerns funding. Three of the paper's four authors are affiliated with Microsoft Research, while *Data Feminism* points to Microsoft's decision to accept a $480 million Department of Defense contract to develop military applications of its HoloLens headset. What happens when research begins to challenge the power of the institutions that fund it? And which research never happens because no one is willing to fund it?

## References

1. Blodgett, S. L., Barocas, S., Daumé III, H., & Wallach, H. (2020). *Language (Technology) is Power: A Critical Survey of "Bias" in NLP.* [arXiv:2005.14050](https://doi.org/10.48550/arXiv.2005.14050)
2. D'Ignazio, C., & Klein, L. F. (2020). *Data Feminism.* Cambridge, MA: MIT Press. ISBN 9780262044004.
