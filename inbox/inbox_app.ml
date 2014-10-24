(** Contains the basic configuration details for an Inbox app. A
 *  self-hosted one needs to specify the api_uri and base_uri 
 *  as well as the secrets.
 *)
type t = {
  api_uri    : Uri.t;
  base_uri   : Uri.t;
  app_id     : string;
  app_secret : string;
}

let make_hosted ~app_id ~app_secret = {
  api_uri  = Uri.of_string "https://api.inboxapp.com";
  base_uri = Uri.of_string "https://www.inboxapp.com";
  app_id; app_secret
}
