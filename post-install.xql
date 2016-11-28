xquery version "3.0";

import module namespace xmldb      = "http://exist-db.org/xquery/xmldb";
import module namespace sm         = "http://exist-db.org/xquery/securitymanager";

(: The following external variables are set by the repo:deploy function :)
(: the target collection into which the app is deployed :)
declare variable $target external;

let $create := xmldb:create-collection($target, "temp")
let $chmod  := sm:chmod($target || "/temp", "777")
