import React from 'react'
import { Gig } from '../model/Model'
import { ProjectComponent } from './Project'

interface Props {
  gig: Gig
}

const Styling = () => (
  <style jsx>{`
    section {
      max-width: 700px;
      margin: 30px auto;
      box-sizing: border-box;
      background: white;
      padding: 50px;
      border: solid 2px #f4f4f0;
    }
  `}</style>
)

export function GigComponent(props: Props) {
  const { gig } = props
  const { title, when, projects, note } = gig
  return (
    <>
      <Styling />
      <section id={title}>
        <h2>
          {title}
          <br />
          <small>
            {when[0]} — {when[1]}
          </small>
        </h2>
        {note && (
          <div style={{ textAlign: 'right' }}>
            <small>{note}</small>
          </div>
        )}
        <hr />
        <ul>
          {projects.map((project, ix) => (
            <>
              {ix !== 0 && <hr />}
              <ProjectComponent key={gig.title} gig={gig} project={project} />
            </>
          ))}
        </ul>
      </section>
    </>
  )
}
