// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}




#let article(
  title: none,
  subtitle: none,
  authors: none,
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: none,
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: none,
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  mathfont: none,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  // Set document metadata for PDF accessibility
  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()
  set par(
    justify: true,
    leading: linestretch * 0.65em
  )
  set text(lang: lang,
           region: region,
           size: fontsize)
  set text(font: font) if font != none
  show math.equation: set text(font: mathfont) if mathfont != none
  show raw: set text(font: codefont) if codefont != none

  set heading(numbering: sectionnumbering)

  show link: set text(fill: rgb(content-to-string(linkcolor))) if linkcolor != none
  show ref: set text(fill: rgb(content-to-string(citecolor))) if citecolor != none
  show link: this => {
    if filecolor != none and type(this.dest) == label {
      text(this, fill: rgb(content-to-string(filecolor)))
    } else {
      text(this)
    }
   }

  let has-title-block = title != none or (authors != none and authors != ()) or date != none or abstract != none
  if has-title-block {
    place(
      top,
      float: true,
      scope: "parent",
      clearance: 4mm,
      block(below: 1em, width: 100%)[

        #if title != none {
          align(center, block(inset: 2em)[
            #set par(leading: heading-line-height) if heading-line-height != none
            #set text(font: heading-family) if heading-family != none
            #set text(weight: heading-weight)
            #set text(style: heading-style) if heading-style != "normal"
            #set text(fill: heading-color) if heading-color != black

            #text(size: title-size)[#title #if thanks != none {
              footnote(thanks, numbering: "*")
              counter(footnote).update(n => n - 1)
            }]
            #(if subtitle != none {
              parbreak()
              text(size: subtitle-size)[#subtitle]
            })
          ])
        }

        #if authors != none and authors != () {
          let count = authors.len()
          let ncols = calc.min(count, 3)
          grid(
            columns: (1fr,) * ncols,
            row-gutter: 1.5em,
            ..authors.map(author =>
                align(center)[
                  #author.name \
                  #author.affiliation \
                  #author.email
                ]
            )
          )
        }

        #if date != none {
          align(center)[#block(inset: 1em)[
            #date
          ]]
        }

        #if abstract != none {
          block(inset: 2em)[
          #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
          ]
        }
      ]
    )
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  doc
}

#set table(
  inset: 6pt,
  stroke: none
)
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "a4",
  margin: (bottom: 1cm,left: 1.4cm,right: 1.2cm,top: 0.8cm,),
  numbering: "1",
  columns: 1,
)

#show: doc => article(
  toc_title: [Table of contents],
  toc_depth: 3,
  doc,
)

#import "@preview/pro-academic-cv:0.1.0": *
#import "@preview/cuti:0.4.0": fakesc

// Footer - set BEFORE resume.with so it does not force a page break
#set page(footer: context [
  #line(length: 100%, stroke: 0.5pt + rgb("cbd5e0"))
  #v(0.35em)
  #grid(columns: (1fr, auto), gutter: 1em, [
    #text(size: 7.5pt, tracking: 0.05em, fill: rgb("718096"))[#fakesc[Aleem Ashraf -- Curriculum Vitae]]
  ], [
    #text(size: 7.5pt, fill: rgb("718096"))[Page ]
    #text(size: 7.5pt, weight: "semibold", fill: rgb("2b6cb0"))[#counter(page).display()]
    #text(size: 7.5pt, fill: rgb("718096"))[ of ]
    #text(size: 7.5pt, fill: rgb("718096"))[#counter(page).final().first()]
  ])
])

#show: resume.with(
  margin: (left: 0.65in, right: 0.65in, top: 0.65in, bottom: 0.65in),
  font-settings: (font-family: "Corbel", font-size: 10pt, author-font-size: 25pt, lang: "en"),
  author-info: (
    name: "Aleem Ashraf",
    primary-info: [Psychology Educator and Quantitative Researcher],
    secondary-info: [#link("https://www.linkedin.com/in/aleemashraf")[#text(fill: rgb("005cb2"))[#link-icon() LinkedIn]] | #link("https://orcid.org/0000-0001-7600-6255")[#text(fill: rgb("005cb2"))[#link-icon() ORCID]] | #link("mailto:someone@somewhere.com")[#text(fill: rgb("005cb2"))[#link-icon() someone\@somewhere.com]] | #link("tel:+921234567890")[#text(fill: rgb("005cb2"))[#link-icon() +92 123 456 7890]]],
    tertiary-info: "Karachi, Pakistan",
  ),
  author-position: left,
)

// Global Link Styling (Professional Blue)
#show link: set text(fill: rgb("005cb2"))

// Global alignment - left-aligned (ragged right) for whole CV, no forced justification
#set par(justify: false)

// Major headings (level 2) - Dark Navy + Enforced Spacing
#show heading.where(level: 2): set text(fill: rgb("1a365d"))
#show heading.where(level: 2): set block(below: 1.2em)

// Subsection headings (level 3) - Small caps with subtle royal blue tint
#show heading.where(level: 3): it => [
  #block(breakable: false, above: 1.0em, below: 0.35em)[
    #fakesc[#text(size: 0.95em, weight: "semibold", tracking: 0.04em, fill: rgb("2b6cb0"))[#it.body]]
  ]
]
#v(2.2em, weak: true)
#block(breakable: false)[
== Professional Profile

I am a psychology educator, researcher, and PhD candidate, committed to advancing competency-based psychology education through evidence-based teaching, quantitative reasoning, and authentic learning experiences. My expertise integrates quantitative research methods, data analysis, psychological assessment, scientific writing, and undergraduate research supervision, enabling students to develop the analytical, critical thinking, and professional competencies essential for contemporary psychological practice. Guided by research in educational psychology, I design technology-enhanced, project-based learning environments that bridge theory with real-world application while fostering scientific reasoning, collaboration, and lifelong learning.
]

#v(1.0em)
#block(inset: (left: 1.5em))[
#grid(
  columns: (1.7in, 1fr),
  gutter: 0.8em,
  row-gutter: 0.7em,
  [#text(size: 1em, weight: "semibold", tracking: 0.04em, fill: rgb("2b6cb0"))[#fakesc[Research Interests]]],
  [#par(justify: false)[control-value theory, achievement emotions, cognitive processes, learning strategies, motivation, moral reasoning]],
  [#text(size: 1em, weight: "semibold", tracking: 0.04em, fill: rgb("2b6cb0"))[#fakesc[Methodological Expertise]]],
  [#par(justify: false)[General Linear Modeling (GLM), Multilevel Modelling (MLM), Structural Equation Modelling (SEM), Latent Profile Analysis (LPA)]],
  [#text(size: 1em, weight: "semibold", tracking: 0.04em, fill: rgb("2b6cb0"))[#fakesc[Software]]],
  [#par(justify: false)[#box(fill: rgb("edf2f7"), radius: 0.35em, inset: (x: 0.6em, y: 0.25em))[#text(size: 0.85em, fill: rgb("2d3748"))[R]] #h(0.35em) #box(fill: rgb("edf2f7"), radius: 0.35em, inset: (x: 0.6em, y: 0.25em))[#text(size: 0.85em, fill: rgb("2d3748"))[IBM SPSS]] #h(0.35em) #box(fill: rgb("edf2f7"), radius: 0.35em, inset: (x: 0.6em, y: 0.25em))[#text(size: 0.85em, fill: rgb("2d3748"))[Jamovi]] #h(0.35em) #box(fill: rgb("edf2f7"), radius: 0.35em, inset: (x: 0.6em, y: 0.25em))[#text(size: 0.85em, fill: rgb("2d3748"))[JASP]]]]
)
]
#v(2.2em, weak: true)
#block(breakable: false)[
== Academic Experience

#v(0.5em)
#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [DOW University of Health Sciences (DUHS)],
    top-right: [May 2025 – Present],
    bottom-left: [Visiting Faculty · Dr. AQ Khan Institute of Behavioral Sciences],
    bottom-right: [],
  )
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Instructed Statistics in Psychology through an applied, lab-based approach across 2 sections, covering 52 students]
- #text(size: 0.95em, fill: rgb("2d3748"))[Emphasised hands-on data analysis, scientific reasoning, statistical thinking, and the interpretation of research findings in psychological contexts]
]
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Pakistan Association of Cognitive Therapy (PACT)],
    top-right: [Oct 2026 – Present],
    bottom-left: [Mentor · Research Training Program],
    bottom-right: [],
  )
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Training and mentoring psychology scholars in quantitative methods and data analysis for sound research]
- #text(size: 0.95em, fill: rgb("2d3748"))[Developing practical competence, critical thinking, through evidence-based, hands-on training in data analysis]
- #text(size: 0.95em, fill: rgb("2d3748"))[Building a research culture grounded in research integrity, rigorous methodology and locally generated psychological evidence]
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Mohammad Ali Jinnah University (MAJU)],
    top-right: [Feb 2022 – Jul 2026],
    bottom-left: [Department of Psychology],
    bottom-right: [],
  ) \
  #row2col([Senior Lecturer in Psychology], [Oct 2025 – Jul 2026]) \
  #row2col([Lecturer in Psychology], [Feb 2022 – Oct 2025]) \
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Supervised 33 students across 11 final-year projects, training them to generate research questions, align research design and statistical analyses with research questions, and limit conclusions within the scope of the study]
- #text(size: 0.95em, fill: rgb("2d3748"))[Taught Quantitative Research Tools via Passion-Driven Statistics model, guiding 133 students across 4 terms in \~28 projects using real-word datasets, building quantitative reasoning and statistical analysis skills]
- #text(size: 0.95em, fill: rgb("2d3748"))[Supervised \~626 final-year students across 396 assessment projects in 8 terms, 224 personality and 172 psychoeducational, with iterative individualised feedback building psychological testing & report writing competence]
- #text(size: 0.95em, fill: rgb("2d3748"))[Taught Mental Health & Psychopathology I-II to 327 students across 7 terms, using role-plays, differential diagnosis exercises, and movie analyses to teach symptom identification, diagnosis, and treatment mapping]
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Pakistan Society for the Excellence in Psychology (PSEP)],
    top-right: [Sep 2021 – May 2022],
    bottom-left: [Rapporteur],
    bottom-right: [],
  )
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Co-authored a practical guide for psychology educators and program leads to implement competency-based education in psychology in accordance with APA Undergraduate Education Guidelines]
- #text(size: 0.95em, fill: rgb("2d3748"))[Prepared meeting minutes, documenting key discussions, decisions, and action items through real-time note-taking and synthesis]
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Institute of Clinical Psychology (ICP)],
    top-right: [Jun 2019 – Feb 2022],
    bottom-left: [University of Karachi],
    bottom-right: [],
  ) \
  #row2col([Research Assistant], [Jul 2020 – Feb 2022]) \
  #row2col([Teaching Associate], [Aug 2019 – Feb 2022]) \
  #row2col([Teaching Assistant], [Jun 2019 – Jul 2019]) \
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Provided clinical supervision in psychological assessment and evidence-based case conceptualisation to postgraduate scholars]
- #text(size: 0.95em, fill: rgb("2d3748"))[Instructed hands-on Advance Psychological Statistics course, deploying evidence-based teaching methodologies to foster statistical reasoning]
- #text(size: 0.95em, fill: rgb("2d3748"))[Peer reviewed or copyedited \~30 research articles submitted to the Pakistan Journal of Psychology (ISSN-E: 2788-4880), helping authors improve the scientific merit and methodological rigor of manuscripts]
]
]
]
#v(2.2em, weak: true)
#block(breakable: false)[
== Research Supervision

#v(0.5em)
#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(justify: false, hanging-indent: 1.5em)[Event Plausibility and Fantasy Proneness in False Memory Formation: An Experimental Test Using a Modified DRM Paradigm · _Student 1, Student 2, Student 3_]],
    align(right)[2026]
  )
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(justify: false, hanging-indent: 1.5em)[Social Comparison and Quarterlife Crisis in Young Adults: Social Isolation as a Mediator and Goal-Setting as a Protective Buffer · _Student 1, Student 2, Student 3_]],
    align(right)[2025]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(justify: false, hanging-indent: 1.5em)[Enhancing Student Performance Through Relevance: An Experimental Evaluation of Utility Value Interventions, Value Appraisals, and Achievement Emotions · _Student 1, Student 2, Student 3_]],
    align(right)[2025]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(justify: false, hanging-indent: 1.5em)[Augmented Reality vs. Multimedia Instruction in Science Pedagogy: A Comparative Evaluation of Conceptual Understanding in Middle School Students · _Student 1, Student 2, Student 3_]],
    align(right)[2025]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(justify: false, hanging-indent: 1.5em)[Do Teacher Attributed Informational Cues Influence Students' Moral Reasoning? An Experimental Investigation in Conformity · _Student 1, Student 2, Student 3_]],
    align(right)[2024]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(justify: false, hanging-indent: 1.5em)[Error-Oriented Motivation, Cognitive Flexibility, and Academic Self-Efficacy: Testing the Moderating Role of Teacher Autonomy Support · _Student 1, Student 2, Student 3_]],
    align(right)[2023]
  )
]
]
#v(2.2em, weak: true)
#block(breakable: false)[
== Academic Qualifications

#v(0.5em)
#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Doctor of Philosophy in Psychology (PhD)],
    top-right: [In progress],
    bottom-left: [Department of Psychology, University of Karachi],
    bottom-right: [Karachi],
  )
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Dissertation Title: The Role of Achievement Emotions in Shaping Learning Strategies: A Test of the Control-Value Theory Among Undergraduates, doctoral research conducted under the supervision of Dr. Anila Amber Malik]
- #text(size: 0.95em, fill: rgb("2d3748"))[Investigated the motivational and emotional processes through which undergraduate students' perceptions of instructional experiences influence achievement emotions, learning strategy use, and ultimately academic achievement]
- #text(size: 0.95em, fill: rgb("2d3748"))[Applied confirmatory factor analysis, multilevel linear mixed-effects modelling, and latent structural equation modelling to investigate direct, indirect, and moderating relationships.]
]
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Master of Philosophy in Clinical Psychology (MPhil)],
    top-right: [2019],
    bottom-left: [Institute of Clinical Psychology (ICP), University of Karachi],
    bottom-right: [Karachi],
  )
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Thesis Title: Relationship of Self-esteem and Coping Strategies with Perfectionism: A Determinant of Life Satisfaction, masters research conducted under the supervision of Late Dr. Sarwat Jahan Khanam]
- #text(size: 0.95em, fill: rgb("2d3748"))[Completed rigorous 106-credit-hour coursework with 3rd position, with supervised clinical training in psychological testing and assessment, psychotherapies, intake interviewing, and research methodology.]
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Bachelor Studies in Psychology (BS)],
    top-right: [2015],
    bottom-left: [Department of Psychology, University of Sindh],
    bottom-right: [Jamshoro],
  )
#block(width: 100%, above: 0.6em, inset: (left: 0.8em))[
#set par(justify: false)
- #text(size: 0.95em, fill: rgb("2d3748"))[Consistently ranked among the top students of the batch, bridging an Arts and Humanities foundation with the scientific rigour of psychology]
- #text(size: 0.95em, fill: rgb("2d3748"))[Co-organized HEC-sponsored workshops with faculty, contributing to departmental academic events.]
]
]
]
#v(2.2em, weak: true)
#block(breakable: false)[
== Professional Development

#v(0.5em)
#block(breakable: false)[
#list[
  #r2c2-entry-header(
    top-left: [Doctoral Research and Scholarly Development] + text(weight: "regular")[#h(0.35em)· Career Break],
    top-right: [Jul 2026 – Present],
  )
#block(width: 100%, above: 0.6em, inset: (left: 1.5em))[
#set par(justify: false)
#text(size: 0.95em, fill: rgb("2d3748"))[Dedicated period for doctoral research, dissertation completion, and scholarly development, with a focus on quantitative data analysis, manuscript preparation, and strengthening research and publication contributions in psychology.]]
]
]
]


#v(1.8em, weak: true)
#block(breakable: false)[
=== Continuing Education

#v(1.2em)
#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Multilevel Modeling, Erasmus University Rotterdam #link("https://www.coursera.org/account/accomplishments/verify/P9X3HNQENK71")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[Aug 2026]
  )
]
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Co-reviewing with a mentor (with assignment), Web of Science Academy #link("https://drive.google.com/file/d/1aFW6YzTzEyyMn8PbVpTWchInB8wF6az3/view")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[Mar 2026]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Certified Peer Review Course, Elsevier Researcher Academy #link("https://drive.google.com/file/d/1vVuuD5rBhlKnOP0RmvykDt9CUzXbM17H/view")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[Mar 2026]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Passion-Driven Statistics: Project Based Teaching Tools, Wesleyan University #link("https://drive.google.com/file/d/1IgCxxUKaJW_txd9Twe0XpaPmrO3tCFuQ/view")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[Aug 2024]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Introduction to Structural Equation Modeling, CenterStat #link("https://drive.google.com/file/d/1mQ2WO4A4EXlptPn-iPXstMYLyIGRXxsA/view")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[Aug 2024]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Statistics for Researchers: Understanding Mediation, Moderation and Beyond, University of Virginia #link("https://www.coursera.org/account/accomplishments/verify/F88ZEEKREVSF")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[May 2020]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Philosophy of Science, University of Pennsylvania #link("https://www.coursera.org/account/accomplishments/verify/JA85QRZBCWWF")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[May 2020]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Writing in the Sciences (with Honors), Stanford Online #link("https://www.coursera.org/account/accomplishments/verify/ZH6E8N896WR3")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[Apr 2020]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Structural Equation Modeling (SEM) - SmartPLS: A Hands-on Approach, Institute of Clinical Psychology]],
    align(right)[Nov 2019]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[The Current Development of SEM & Data Analysis Using SmartPLS 3.0, Bahria University]],
    align(right)[Sep 2019]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Discovering Your PhD Potential: Writing a Research Proposal, University of Leicester #link("https://www.futurelearn.com/certificates/70fnjkj")[#text(fill: rgb("005cb2"))[#link-icon()]]]],
    align(right)[Jan 2019]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Transforming a Thesis, Dissertation or Research Study into a Publishable Document in Internationally Rated Journals, Bahria University]],
    align(right)[Jan 2019]
  )
]
]

#v(1.2em, weak: true)

#block(breakable: false)[
#list[
  #grid(
    columns: (1fr, auto),
    gutter: 0.5in,
    align(left)[#par(hanging-indent: 1.5em)[Applying Quantitative Techniques Using SPSS, University of Sindh]],
    align(right)[Apr 2017]
  )
]
]
#v(2.2em, weak: true)
#block(breakable: false)[
== Publications (note:P = Published, S = Submitted)

#grid(
  columns: (auto, 1fr),
  column-gutter: 1.2em,
  row-gutter: 0.8em,
    align(left)[P],
    align(left)[#par(hanging-indent: 1.5em)[Ashraf, A. & Malik, A. (2026). When Feeling in Control Isn’t Enough: How Value Shapes Undergraduate Students’ Boredom. _Qlantic Journal of Social Sciences_, 7(5), 170–182 #link("https://doi.org/10.55737/qjss.vii-v.26610")[#text(fill: rgb("005cb2"))[https://doi.org/10.55737/qjss.vii-v.26610]]]],
    align(left)[P],
    align(left)[#par(hanging-indent: 1.5em)[Ashraf, A., Sarfaraz, A., Hameed, H., Saleem, S., & Kayani, S. (2022). Promoting Competency-Based Teaching and Training in Psychology in Pakistan. _PsychArchives_. #link("https://doi.org/10.23668/psycharchives.22693")[#text(fill: rgb("005cb2"))[https://doi.org/10.23668/psycharchives.22693]]]],
    align(left)[P],
    align(left)[#par(hanging-indent: 1.5em)[Ashraf, A. (2019). Relationship of Self-esteem and Coping Strategies with Perfectionism: A Determinant of Life Satisfaction. [MPhil thesis, University of Karachi]. #link("https://doi.org/10.23668/psycharchives.22692")[#text(fill: rgb("005cb2"))[https://doi.org/10.23668/psycharchives.22692]]]],
    align(left)[S],
    align(left)[#par(hanging-indent: 1.5em)[Ashraf, A., Asif, A., Farooqui, A., & Rizwan, D. (2026). Do Teacher-Attributed Informational Cues Influence Students' Moral Judgments? An Experimental Study of Conformity. Under review in Social Psychology of Education.]],
    align(left)[S],
    align(left)[#par(hanging-indent: 1.5em)[Ashraf, A. (2026). Control- and Value-Related Appraisals to Critical-Thinking Strategy Use: Differentiated Indirect Pathways via Enjoyment and Boredom. Manuscript submitted for publication in Educational Research and Evaluation.]]
)

]



