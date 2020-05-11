import Head from 'next/head'
import React, { ReactNode } from 'react'

type Props = {
  title?: string
  children?: ReactNode
}

export function Layout(props: Props) {
  const { children, title } = props
  return (
    <div>
      <Head>
        <title>{title}</title>
        <meta charSet='utf-8' />
        <meta name='robots' content='noindex' />
        <meta name='viewport' content='width=700px, initial-scale=1.0' />
        <link
          href='https://fonts.googleapis.com/css?family=Open+Sans:400,600&display=swap'
          rel='stylesheet'
          type='text/css'
        />
      </Head>
      <body>{children}</body>
      <script type='text/javascript' dangerouslySetInnerHTML={{ __html: GA }} />
    </div>
  )
}

const GA = `
  var _gaq = _gaq || [];
  _gaq.push(["_setAccount", "UA-38893786-1"]);
  _gaq.push(["_setDomainName", "fvisser.nl"]);
  _gaq.push(["_trackPageview"]);
  (function () {
    var ga = document.createElement("script");
    ga.type = "text/javascript";
    ga.async = true;
    ga.src =
      ("https:" == document.location.protocol ? "https://ssl" : "http://www") +
      ".google-analytics.com/ga.js";
    var s = document.getElementsByTagName("script")[0];
    s.parentNode.insertBefore(ga, s);
  })();
`
