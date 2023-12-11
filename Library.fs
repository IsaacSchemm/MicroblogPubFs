namespace MicroblogPubFs

open System
open System.Net
open System.Net.Http
open FSharp.Data

type ICredentials =
    abstract member Password: string

type NumberedPaging =
| FirstPage
| Page of int

type CursorPaging =
| FirstPage
| Page of string

type Actor = {
    Link: Uri
    IconUri: Uri
    DisplayName: string
    Handle: string
}

type Sharer = {
    Link: Uri
}

type ShareContext = {
    SharedAt: DateTimeOffset
    SharedBy: Sharer
}

type OpenGraphMeta = {
    ImageUris: Uri list
    Link: Uri
    Title: string
}

type AttachedPhoto = {
    Link: Uri
    Thumbnail: Uri
    TitleText: string
}

type AttachedVideo = {
    Src: Uri
    TitleText: string
}

type Post = {
    ShareContext: ShareContext list
    Actor: Actor
    ContentWarnings: string list
    Html: string
    OpenGraphMeta: OpenGraphMeta list
    HasAttachments: bool
    Photos: AttachedPhoto list
    Videos: AttachedVideo list
    Permalink: Uri
    PublishedAt: DateTimeOffset
}

module Util =
    let private innerHtml (node: HtmlNode) = String.concat "" (seq {
        for c in node.Elements() do
            string c
    })

    let private single (selector: string) (parent: HtmlNode) =
        parent.CssSelect(selector)
        |> List.tryExactlyOne
        |> Option.defaultWith (fun () -> failwithf "None found for %s in %s" selector (innerHtml parent))

    let private attrVal (attributeName: string) (node: HtmlNode) =
        node.AttributeValue(attributeName)

    let private innerText (node: HtmlNode) =
        node.InnerText()

    let private parseDate (str: string) =
        DateTimeOffset.Parse($"{str}Z")

    let parseFeed (feedPage: HtmlDocument) = [
        let h_feed = feedPage.Html().CssSelect(".h-entry")
        for h_entry in h_feed do
            let u_url = h_entry |> single ".actor-box .u-url"
            {
                ShareContext = [
                    for shared_header in h_entry.CssSelect(".shared-header") do
                        {
                            SharedAt = shared_header |> single "span" |> attrVal "title" |> parseDate
                            SharedBy = {
                                Link = shared_header |> single "a" |> attrVal "href" |> Uri
                            }
                        }
                ]
                Actor = {
                    Link = new Uri(u_url |> attrVal "href")
                    IconUri = new Uri(h_entry |> single ".actor-box .actor-icon" |> attrVal "src")
                    DisplayName = u_url |> single "strong" |> innerText
                    Handle = u_url |> single ".actor-handle" |> innerText
                }
                ContentWarnings = [
                    for summary in h_entry.CssSelect("summary") do
                        summary.InnerText()
                ]
                Html = h_entry |> single ".e-content" |> innerHtml
                OpenGraphMeta = [
                    for activity_og_meta in h_entry.CssSelect(".activity-og-meta") do
                        {
                            ImageUris = [
                                for img in activity_og_meta.CssSelect("img") do
                                    img |> attrVal "src" |> Uri
                            ]
                            Link = activity_og_meta |> single "a" |> attrVal "href" |> Uri
                            Title = activity_og_meta |> single "a" |> innerText
                        }
                ]
                HasAttachments = h_entry.CssSelect(".attachment-item") <> []
                Photos = [
                    for link in h_entry.CssSelect(".attachment-item a.media-link") do
                        for img in link.CssSelect("img.attachment.u-photo") do
                            {
                                Link = new Uri(link |> attrVal "href")
                                Thumbnail = new Uri(img |> attrVal "src")
                                TitleText = img |> attrVal "title"
                            }
                ]
                Videos = [
                    for video in h_entry.CssSelect(".attachment-item video.u-video") do
                        {
                            Src = new Uri(video |> attrVal "src")
                            TitleText = video |> attrVal "title"
                        }
                ]
                Permalink =
                    h_entry |> single ".object-permalink.u-url" |> attrVal "href" |> Uri
                PublishedAt =
                    h_entry |> single ".dt-published" |> attrVal "datetime" |> parseDate
            }
        ]

module Public =
    let httpClientHandler = new HttpClientHandler()

    httpClientHandler.CookieContainer <- new CookieContainer()
    //httpClientHandler.CookieContainer.Add(new Cookie("session", ".", "/", "microblog.lakora.us"))

    let httpClient = new HttpClient(httpClientHandler)

    let GetFeedAsync paging = task {
        let queryString = String.concat "&" [
            match paging with
            | NumberedPaging.FirstPage -> ()
            | NumberedPaging.Page n -> sprintf "page=%d" n
        ]
        let! resp = httpClient.GetAsync($"https://microblog.lakora.us/?{queryString}")
        resp.EnsureSuccessStatusCode() |> ignore
        let! html = resp.Content.ReadAsStringAsync()
        let feedPage = HtmlDocument.Parse(html)
        return Util.parseFeed feedPage
    }