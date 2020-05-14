import { Portfolio, Gig, Project } from './Model'

const Kanban: Project = {
  description: [
    `Quick prototype project of a Kanban-like productivity app on top of the Wireline decentralized operating environment.
     Running fully in the client without a central server environment.
     All application data is stored and distributed using a series of custom CRDTs, natively supporting concurrent access by multiple users as well as offline support.`
  ],
  visuals: [
    {
      img: 'kanban - board settings',
      description: `Main board overview with access to board settings from the top bar. The board in this screenshot is tracking this project's actual progress.`
    },
    {
      img: 'kanban - card overview',
      description: `Single card overview showing features like tagging, markdown desctiptions and comments, start and due dates and ownership assignment.`
    },
    {
      img: 'kanban - moving list',
      description: `Moving around lists and cards using drag and drop. Including the subtle but obligatory Trello-like rotation of the draggable item.`
    },
    {
      img: 'kanban - card checklist',
      description: `Another card overview showing support for checklists. Progress will be visible on the card thumbnails on the main board.`
    }
  ]
}

const Wireline: Gig = {
  title: 'Wireline',
  when: ['Nov 2019', 'May 2020'],
  note: "Please don't share any of this work publicly.",
  projects: [Kanban]
}

const Fourier: Project = {
  title: 'Fourier',
  description: [
    `Fourier is a graphical user interface for building and maintaining data pipelines. The pipelines get executed on the backend in a cluster of Python compute kernels. The interaction model is twofold: power users write building blocks and define pipeline workflows, end-users perform data analysis by instantiating these predefined workflows.`,
    `Building workflows is done by annotating Python functions with input and output types. A custom inference engine in the frontend makes sure only appropriate pipeline steps are suggested to the user and ensures pipelines are valid by construction. The result data of a pipeline can be written back to the underlying data platform or presented in the UI with special visualization steps.`
  ],
  visuals: [
    {
      img: 'fourier - viz tabs',
      description: `Main Fourier screen with pipeline steps on the left and map visualization in the main output window`
    },
    {
      img: 'fourier - hierarchy',
      description: `Small experiment with branching pipeline steps using a small indent to indicate structure.`
    },
    {
      img: `fourier - function library`,
      description: `The Fourier function library. The list of functions with basic version control status on the left, code editor in the middle, and type + metadata configuration on the right.`
    },
    {
      img: `fourier - logo`,
      description: `Playground for the Fourier and Titan logo design. Titan is a rebrand of Fourier specialized for machine learning pipelines using PySpark + TensorFlow.`
    },
    {
      img: `fourier - repl`,
      description: `Fourier REPL mode where you can execute code directly in the context of the active pipeline step. All data in scope, like the output of the input step are available for debugging purposes. Values that match known visualizations steps can be inspected on the fly.`
    },
    {
      img: `fourier - workflow editor`,
      description: `The workflow editor that allows workflow authors to create pipeline templates by stitching together functions of the right type and adding the necessary metadata.`
    }
  ]
}

const Explorer: Project = {
  title: 'Explorer',
  description: [
    `Explorer is an experimental table-first table processing tool built around the concept of object types with relations forming knowledge graphs. The engine driving all of the UX is a small typed functional programming language specialized for building graph queries and table manipulation operations.`
  ],
  visuals: [
    {
      img: `explore - model table`,
      description: `The main table UI showing typed values including units for numeric values.`
    },
    {
      img: `explore - table editing`,
      description: `Every column represents some pivot in the knowledge graph and adding derived columns is like building a graph query based on a search-around in the meta model.`
    },
    {
      img: `explore - model object`,
      description: `The Explorer object view. Object views allow you to zoom in on specific entries of a single object type. Different types of information can have different visual representations.`
    },
    {
      img: `explore - object editing`,
      description: `The object view at the basis represents a single row from a tabular source. However, different information can be directly pulled in based on context sensitive search-around.`
    },
    {
      img: `explore - map view`,
      description: `An alternative map visualization allows plotting all entities that have associated geo information, either directly on the entity or accessible via some context query.`
    },
    {
      img: `explore - nested table`,
      description: `The table view can display hierarchical information by joining in information from other data sources via predefined relations. When multiple results are returned per row the table is nested.`
    }
  ]
}

const Ontology: Project = {
  title: 'Ontology Editor',
  description: [
    `The ontology editor is a small utility on the Foundry data platform allowing users to author the meta model connected to their data storage. It allows for the configuration of object types, relations and meta information that will drive all the object based UX in the platform. Besides the configuration of semantic information the tool also controls the technical configuration of how exactly raw data is ingested into the object model.`
  ],
  visuals: [
    {
      img: `ontology - app`,
      description: `The main ontology editor window as a standalone application. List of editable object types and relations on the left and the editing panel on the right.`
    },
    {
      img: `ontology - dataset`,
      description: `The ontology editor as an integrated popover in the dataset preview application. This allows users to enrich raw data with an object type and relations directly after ingestion.`
    },
    {
      img: `ontology - auto relation`,
      description: `By scanning a snapshot of the raw data the ontology editor can auto suggest a lot of sane defaults for the object type configuration. Even relations to other object types can be inferred by trying likely join points.`
    },
    {
      img: `ontology - graph`,
      description: `Object types with both incoming and outgoing relations could be inspected in graph form to gain a better understanding of your data model. Clicking on the related nodes would scroll and focus that object type so you could incrementally walk the graph.`
    }
  ]
}

const Hubble: Project = {
  title: 'Hubble',
  description: [`Hubble is yet another object based data analysis tool.`],
  visuals: [
    {
      img: `hubble - icon`,
      description: `File icon design for Hubble dashboards links in the file explorer.`
    },
    {
      img: `hubble - concept`,
      description: `Concept mock of an table based object explorer as an alternative to Hubble's original presentation and analysis approach. This mock formed the inspiration for the Explorer project outlined above.`
    }
  ]
}

const Tandem: Project = {
  title: 'Tandem',
  description: [
    `Tandem is a tool for managing the vast amount of work orders involved in aircraft manufacturing. All individual steps involved in the logistics, planning and assembly could be tracked and possible rescheduled.`
  ],
  visuals: [
    {
      img: `tandem - main`,
      description: `First experimentation with a sidebar UI for views and folders.`
    },
    {
      img: `tandem - add filter`,
      description: `Experimenting with an alternative UI for adding new filters to a Tandem view. Instead of using small dropdown with many items in a small scroll box we tried laying out all possible filters in view at once and allow a quick search through them.`
    }
  ]
}

const Palantir: Gig = {
  title: 'Palantir',
  when: ['Aug 2016', 'Jul 2019'],
  note: "Please don't share any of this work publicly.",
  projects: [Fourier, Explorer, Ontology, Hubble, Tandem]
}

const Silk: Gig = {
  title: 'Silk',
  when: ['Jul 2009', 'Aug 2016'],
  projects: [
    {
      description: [
        `Silk is a web applications that allows users to go from raw spreadsheets to beautiful structured websites in minutes. A Silk site feels like a wiki where all information is directly accessible on the web, but were all the factual information about pages (sometimes called data-cards) are indexed into a structured database. So, besides a beautiful presentation and WYSIWYG editing experience, all your data is accessible for structured analysis, exploration and visualization as well.`,
        `After the acquisition by Palantir in 2016 and a nearly two year transition period Silk was finally shutdown in 2018. As a founding engineer I have been involved in many aspects of the product development process, with the main responsibility for product design and user interaction.`
      ],
      visuals: [
        {
          img: `explore - map pie charts`,
          description: `Numeric distributions as mini pie charts on a map visualization in explore view. Hover actions show details of the underlying data, clicking opens a specific chart with more detailed information.`
        },
        {
          img: `explore - suggestions`,
          description: `Grid of visualization suggestions in Silk's explore mode. Suggestions are based on simple heuristics taking input from existing visualizations, data inference, explicit starring actions, and implicit user behavior.`
        },
        {
          img: `intelli search`,
          description: `Popover search box for Silk quasi-natural language semantic search engine. The input box takes a search string and tries to output a matching part of the knowledge graph in an appropriate presentation.`
        },
        {
          img: `page - topbar and layout`,
          description: `Initial designs for a cleaner Silk page layout. The design is based on wider visualizations more clearly contrasting the regular text flow of the page. The top bar is an attempt to clean up the page actions into visually less cluttered form.`
        },
        {
          img: `filtering - range filter`,
          description: `Exploratory design work for numeric range filters used in Silk's visualization builder. Auto-bucketing numeric values into sensible ranges makes input easier and gives users a sense of the data distribution at the same time.`
        },
        {
          img: `importer - configure`,
          description: `Importing raw data from a spreadsheet into Silk used to be a rather opaque process and it was hard to judge the outcome before starting the long running process. This preview based import wizard greatly helped managing our user's expectations and exposed possible issues upfront.`
        },
        {
          img: `timeline`,
          description: `Early draft mock of the Silk activity feed. The feed shows which users have been working on parts of site you might be interested in. Unfortunately, the activity feed has never made it into production.`
        },
        {
          img: `filter - workflow`,
          description: `This design shows the workflow of creating a new filter attached to a Silk visualization. The filter type (numeric range / exists / does not exist / textual match / pick from enumeration) is based on the inferred data type for the selected property. The new filter UI was based on small embeddable pills with the filter configuration as a popover.`
        },
        {
          img: `page - editing`,
          description: `A Silk data-card in edit mode. Edit mode allows users to add new widgets and content to an existing page in WYSIWYG fashion. The factsheet widget allows adding key/values to a card directly indexed into the structured database.`
        },
        {
          img: `visualization - linechart embed`,
          description: `Preview of a Silk visualization as it would look when embedded into 3rd party websites. Users could pick a subsets of the visualization's filters and expose them interactively on the embedded widget.`
        },
        {
          img: `visualization - popup`,
          description: `The Silk visualization editor as a popup on top of the existing page UI, instead of opening a new view to do the same thing. Even as a popup there usually is enough space for a configuration and filter bar.`
        },
        {
          img: `page - section config`,
          description: `The page section configuration UI. This popup allows users to customize the look and feel of Silk pages. Pages could have multiple sections, containing a flexible column layout and distinctive visual style.`
        },
        {
          img: `twitter card icon`,
          description: `Icon design for use in Open Graph Twitter cards.`
        },
        {
          img: `rnd - chart axis`,
          description: `Research project into figuring out a consistent placements of axis labels on numeric charts. Trying to find a good trade-off between space efficiency and legibility.`
        },
        {
          img: `signin page`,
          description: `Mocks for the new signup page that allows users to create an account and a first Silk site in a single pass. One step, one button and you're ready to import data or start editing pages manually.`
        },
        {
          img: `rnd - timeseries`,
          description: `Another small research project into axis label placement
            specifically for times series.`
        }
      ]
    }
  ]
}

export const MyPortfolio: Portfolio = {
  gigs: [Wireline, Palantir, Silk]
}
