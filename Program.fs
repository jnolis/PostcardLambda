namespace PostcardLambda

open System
open System.IO
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Net.Http
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Amazon.S3
open Amazon.S3.Model
open Amazon.Lambda.Core

type MiniPhotoInfo = 
    {
        Id: int64
        Secret: string
        OriginalFormat: string
    }

type PhotoInfo = 
    {
        Id: int64
        Secret: string
        OriginalSecret: string
        Server: string
        Farm: string
        OwnerFullName: string
        OwnerUsername: string
        OriginalFormat: string
        Title: string
        PublicUrl: string option
    }

module Program =


    let apiKey =
        "config.json"
        |> File.ReadAllText
        |> JObject.Parse
        |> (fun x -> string x.["ApiKey"])

    let getPhoto (apiKey: string) (photoInfo: PhotoInfo) =
        let url = 
            "https://farm" + photoInfo.Farm + 
            ".staticflickr.com/" + photoInfo.Server + 
            "/" + photoInfo.Id.ToString() + 
            "_" + photoInfo.OriginalSecret + 
            "_o." + photoInfo.OriginalFormat
        use client = new HttpClient()
        use response = 
            client.GetAsync(url)
            |> Async.AwaitTask
            |> Async.RunSynchronously
        let stream =  
            response.Content.ReadAsStreamAsync()
            |> Async.AwaitTask
            |> Async.RunSynchronously
        Image.Load<Rgba32>(stream)  

    let getJson (url:string) = 
        let fixFlickrJson (s:string) = s.Substring(14,s.Length-15)

        use client = new HttpClient()
        use response = 
            client.GetAsync(url)
            |> Async.AwaitTask
            |> Async.RunSynchronously
        
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> fixFlickrJson
    
    let getPhotoSize (apiKey) (id:int64) = 
        let url = 
            "https://api.flickr.com/services/rest/?method=flickr.photos.getSizes" + 
            "&api_key=" + apiKey + 
            "&photo_id=" + id.ToString() +
            "&format=json"
        let j = JObject.Parse (getJson(url))
        j.["sizes"].["size"]
        |> Seq.tryFind (fun s -> string s.["label"] = "Original")
        |> Option.map (fun s -> (int s.["width"], int s.["height"]))

    let getPhotoInfo (apiKey) (id:int64) (secret:string) = 
        let url = 
            "https://api.flickr.com/services/rest/?method=flickr.photos.getInfo" + 
            "&api_key=" + apiKey + 
            "&photo_id=" + id.ToString() +
            "&secret=" + secret +
            "&format=json"
        let j = JObject.Parse (getJson(url))
        {
            Id = id
            Secret = secret
            Server = string j.["photo"].["server"]
            Farm = string j.["photo"].["farm"]
            OriginalSecret = string j.["photo"].["originalsecret"]
            OriginalFormat = string j.["photo"].["originalformat"]
            OwnerUsername = string j.["photo"].["owner"].["username"]
            OwnerFullName = string j.["photo"].["owner"].["realname"]
            Title = string j.["photo"].["title"].["_content"]
            PublicUrl = 
                j.["photo"].["urls"].["url"]
                |> Seq.tryFind (fun u -> (string u.["type"]) = "photopage")
                |> Option.map (fun u -> string u.["_content"])
        }

    let getPhotos (apiKey) (tags: string list) =
        let getPhotoPage (apiKey) (tags: string list) (perPage: int) (page: int option) =
            let url = 
                "https://api.flickr.com/services/rest/?method=flickr.photos.search" +
                "&api_key=" + apiKey + 
                "&tags=" + (List.reduce (fun x y -> x + "," + y) tags) +  
                "&sort=relevance&content_type=1&license=2&format=json" + 
                "&extras=original_format" + 
                "&per_page=" + perPage.ToString() +  
                "&page=" + (match page with | Some p -> p.ToString() | None -> "1")
            getJson(url)
            |> JObject.Parse
            |> (fun j -> j.["photos"].["photo"])
            |> Seq.map (fun p -> {
                                    MiniPhotoInfo.Id = p.["id"].ToObject<int64>();
                                    MiniPhotoInfo.Secret = p.["secret"].ToObject<string>();
                                    MiniPhotoInfo.OriginalFormat = p.["originalformat"].ToObject<string>();   
                                })
            |> List.ofSeq

        let getPhotoPageCount (apiKey) (tags: string list) (perPage: int) =
            let url = 
                "https://api.flickr.com/services/rest/?method=flickr.photos.search" +
                "&api_key=" + apiKey + 
                "&tags=" + (List.reduce (fun x y -> x + "," + y) tags) +  
                "&sort=relevance&content_type=1&license=2&format=json" + 
                "&per_page=" + perPage.ToString()
            getJson(url)
            |> JObject.Parse
            |> (fun j -> j.["photos"].["pages"].ToObject<int>())
        
        let perPage = 25
        let pageCount = getPhotoPageCount apiKey tags perPage
        let page = System.Random().Next(max 1 pageCount/4)
        System.Console.WriteLine (page.ToString())
        getPhotoPage apiKey tags perPage (Some page)
        
        
    let isGoodSizePhoto desiredWidth desiredHeight (width:int) (height:int) = 
        width >= desiredWidth &&
        height >= desiredHeight &&
        ((float width)/(float height) > 1.4) && 
        ((float width)/(float height) < 1.6)

    let getGoodPhotos (tries: int) (apiKey: string) (tags: string list) desiredWidth desiredHeight= 
        let getGoodPhotoAttempt apiKey tags =
            getPhotos apiKey tags
            |> Array.ofList
            |> Array.Parallel.choose (fun mp -> 
                  let size = getPhotoSize apiKey mp.Id
                  let format = mp.OriginalFormat
                  match (size,format) with
                  | (Some (w,h),f) when isGoodSizePhoto desiredWidth desiredHeight w h && f = "jpg"-> Some mp
                  | _ -> None)         
            |> List.ofArray
        Seq.init tries (fun i -> getGoodPhotoAttempt apiKey tags)
        |> Seq.skipWhile List.isEmpty
        |> Seq.tryHead
        |> (fun x -> match x with | Some y -> y | None -> List.empty)

    let resize (width: int) (height: int) (p:Image<Rgba32>) =
        let ratio = max ((float width)/(float p.Width)) ((float height)/(float p.Height))
        let adjust x = x |> float |> (*) ratio |> ceil |> int
        System.Console.WriteLine (p.Width.ToString() + "x" + p.Height.ToString() + " -> " + (adjust p.Width).ToString() + "x" + (adjust p.Height).ToString()) 
        p.Mutate(fun x -> x.Resize(SixLabors.Primitives.Size(adjust p.Width,adjust p.Height), Lanczos5Resampler(), false) |> ignore)
        p.Mutate(fun x -> x.Crop(SixLabors.Primitives.Rectangle(0,0, width, height)) |> ignore)
        p

    let desiredWidth = 1875
    let desiredHeight = 1275        

    let photo =
        getGoodPhotos 5 apiKey (List.singleton "flowers") desiredWidth desiredHeight
        |> (fun x->
            if List.isEmpty x then failwith "No pictures found :("
            else x
            )
        |> (fun l -> List.item (System.Random().Next l.Length) l)
        |> (fun mp -> getPhotoInfo apiKey mp.Id mp.Secret)
        |> (fun p -> getPhoto apiKey p)
        |> resize desiredWidth desiredHeight

    let handler(context:ILambdaContext) =
        let memoryStream = new MemoryStream()
        photo.SaveAsJpeg(memoryStream)
        let client = new AmazonS3Client(Amazon.RegionEndpoint.USWest2)
        let putRequest = PutObjectRequest(BucketName="postcard-lambda",Key = System.Guid.NewGuid().ToString(),ContentType="image/jpeg",InputStream=memoryStream)
        client.PutObjectAsync putRequest
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> ignore

    [<EntryPointAttribute>]
    let main args =
        let fileStream = new FileStream(args.[0],FileMode.Create)
        photo.SaveAsJpeg(fileStream)
        fileStream.Dispose()
        0