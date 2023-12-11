namespace MicroblogPubFs

open System
open System.Net
open System.Net.Http
open FSharp.Data

type Actor = {
    Link: Uri
    IconUri: Uri
    DisplayName: string
    Handle: string
}

type Sharer = {
    Link: Uri
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

type ActivityBarAction = {
    Action: string
    ApObjectId: string
    CsrfToken: string
    SubimtValue: string
}

type Post = {
    SharedAt: Nullable<DateTimeOffset>
    IsReply: bool
    Actor: Actor
    ContentWarning: string
    Html: string
    HasAttachments: bool
    Photos: AttachedPhoto list
    Permalink: Uri
    PublishedAt: DateTimeOffset
    Actions: ActivityBarAction list
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
                SharedAt =
                    match h_entry.CssSelect(".shared-header") with
                    | shared_header::_ ->
                        Nullable (shared_header |> single "span" |> attrVal "title" |> parseDate)
                    | [] ->
                        Nullable()
                IsReply = h_entry.CssSelect(".in-reply-to") <> []
                Actor = {
                    Link = new Uri(u_url |> attrVal "href")
                    IconUri = new Uri(h_entry |> single ".actor-box .actor-icon" |> attrVal "src")
                    DisplayName = u_url |> single "strong" |> innerText
                    Handle = u_url |> single ".actor-handle" |> innerText
                }
                ContentWarning =
                    match h_entry.CssSelect("summary") with
                    | summary::_ -> summary.InnerText()
                    | _ -> null
                Html = h_entry |> single ".e-content" |> innerHtml
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
                Permalink =
                    h_entry |> single ".object-permalink.u-url" |> attrVal "href" |> Uri
                PublishedAt =
                    h_entry |> single ".dt-published" |> attrVal "datetime" |> parseDate
                Actions = [
                    for form in h_entry.CssSelect(".activity-bar form") do
                        if form |> attrVal "method" = "POST" then
                            {
                                Action = form |> attrVal "action"
                                ApObjectId = form |> single "input[name=ap_object_id]" |> attrVal "value"
                                CsrfToken = form |> single "input[name=csrf_token]" |> attrVal "value"
                                SubimtValue = form |> single "input[type=submit]" |> attrVal "value"
                            }
                ]
            }
        ]

module Public =
    let httpClientHandler = new HttpClientHandler()

    httpClientHandler.CookieContainer <- new CookieContainer()
    //httpClientHandler.CookieContainer.Add(new Cookie("session", ".", "/", "microblog.lakora.us"))

    let httpClient = new HttpClient(httpClientHandler)

    let GetFeedAsync page = task {
        let queryString = String.concat "&" [
            sprintf "page=%d" page
        ]
        let! resp = httpClient.GetAsync($"https://microblog.lakora.us/?{queryString}")
        resp.EnsureSuccessStatusCode() |> ignore
        let! html = resp.Content.ReadAsStringAsync()
        let feedPage = HtmlDocument.Parse(html)
        return Util.parseFeed feedPage
    }