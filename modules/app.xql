xquery version "3.0";

module namespace app = "https://xtriples.lod.academy/templates";

import module namespace templates = "http://exist-db.org/xquery/templates" ;
import module namespace config = "https://xtriples.lod.academy/config" at "config.xqm";
import module namespace functx = "http://www.functx.com";

(: returns a link with base url to current app location prepended :)
declare function app:link($node as node(), $model as map(*), $href as xs:string, $class as xs:string) {
	let $baseUrl := functx:substring-before-last($config:xtriplesWebserviceURL, '/')
	let $link := <a href="{replace($href, "\$baseUrl", $baseUrl)}" class="{replace($class, "\$baseUrl", $baseUrl)}">{$node/text()}</a>
return $link
};