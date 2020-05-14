import { GigComponent } from '../components/Gig'
import { Layout } from '../components/Layout'
import { MyPortfolio } from '../model/Data'
import { Portfolio } from '../model/Model'
import { Nav } from '../components/Nav'

interface Props {
  portfolio: Portfolio
}

const Styling = () => (
  <style jsx>{`
    body {
      font-family: 'open sans', sans-serif;
      background: #f4f4f0;
      font-size: 15px;
      padding: 0;
      margin: 0;
    }
    a {
      color: black;
    }
    small {
      color: rgba(0, 0, 0, 0.6);
      font-size: 80%;
    }
    .small a {
      color: rgba(0, 0, 0, 0.6);
    }
    h1,
    h2 {
      text-transform: uppercase;
      text-align: center;
      font-weight: 500;
    }
    h1 {
      color: #530;
      margin: 50px 0 50px 0;
    }
    h2 {
      color: #005;
      margin: 20px 0 50px 0;
      letter-spacing: 0.1em;
    }
    h1 small {
      letter-spacing: 0;
    }
    a {
      color: black;
    }
    .role {
      font-weight: bold;
    }
    ul {
      list-style: none;
      padding: 0;
    }
    li {
      margin-bottom: 50px;
    }
    hr {
      display: block;
      border: none;
      height: 2px;
      background: #f4f4f0;
      width: calc(100% - 0px);
      margin: 0 auto 50px auto;
    }
    small {
      color: rgba(0, 0, 0, 0.6);
      font-size: 80%;
    }
    .small a {
      color: rgba(0, 0, 0, 0.6);
    }
    .desc {
      color: rgba(0, 0, 0, 0.7);
      margin-bottom: 40px;
    }
  `}</style>
)

const IndexPage = ({ portfolio }: Props) => (
  <>
    <Styling />
    <Layout title='UX Portfolio - Sebastiaan Visser'>
      <Nav portfolio={portfolio} />
      {portfolio.gigs.map(gig => (
        <GigComponent gig={gig} />
      ))}
    </Layout>
  </>
)

export default IndexPage

export async function getStaticProps(): Promise<{ props: Props }> {
  const props: Props = {
    portfolio: MyPortfolio
  }
  return { props }
}
