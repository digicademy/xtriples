xquery version "3.0";

(:~
 : XTriples
 :
 : A generic webservice to extract rdf statements from XML resources
 :
 : @author Torsten Schrade
 : @email <Torsten.Schrade@adwmainz.de>
 : @version 1.4.0
 : @licence MIT
 :
 : Main module containing the webservice
:)



(: ########## PROLOGUE ############################################################################# :)

import module namespace functx      = "http://www.functx.com";
import module namespace console     = "http://exist-db.org/xquery/console";
import module namespace httpclient  = "http://exist-db.org/xquery/httpclient";
import module namespace request     = "http://exist-db.org/xquery/request";
import module namespace response    = "http://exist-db.org/xquery/response";
import module namespace transform   = "http://exist-db.org/xquery/transform";
import module namespace util        = "http://exist-db.org/xquery/util";
import module namespace xmldb       = "http://exist-db.org/xquery/xmldb";

import module namespace config      = "http://xtriples.spatialhumanities.de/config" at "modules/config.xqm";

declare namespace xtriples = "https://xtriples.lod.academy/";

declare variable $setConfiguration  := xtriples:getConfiguration();
declare variable $setFormat         := xtriples:getFormat();



(: ########## DEBUGGING FUNCTIONS ################################################################# :)



declare function local:log($message as xs:string, $priority as xs:string?) {
	let $prio := if ($priority) then $priority else "trace"
	return try  {
					let $consoleOutput  := if ($config:debug = "trace" or $prio = ("info", "warn", "error")) then
												console:log($message)
											  else ()
					let $fileOutput	  := local:logToFile($prio, $message)
					return true()
				}
			catch *
				{
					($err:code, $err:description)
				}
};

declare function local:logToFile($priority as xs:string, $message as xs:string) {
	let $file	:= $config:logfile
	return  (
				let $log := util:log-app($priority, $file, $message)
				return if ($config:debug = "trace" or $priority = ('error', 'warn')) then
					util:log-system-out($message)
				else ()
			)
};



(: ########## CONFIGURATION FUNCTIONS ############################################################## :)



(: retrieves the XTriples configuration from GET or POST :)
declare function xtriples:getConfiguration() {

	(: checks GET/POST for a configuration sent with the configuration parameter :)
	let $submittedConfiguration := request:get-parameter("configuration", "")
	(: checks the request body for a configuration :)
	let $submittedConfigurationPOSTBody := request:get-data()

	let $setConfiguration :=
		(: case 1 - URI to a configuration file was sent - GET request :)
		if (substring($submittedConfiguration, 1, 4) = "http") then 
			fn:doc(xs:anyURI($submittedConfiguration))
		(: case 2 - nothing submitted with a configuration parameter, use request body - direct POST request :)
		else if ($submittedConfiguration = "") then
			if ($submittedConfigurationPOSTBody) then
				$submittedConfigurationPOSTBody
			(: if no configuration could be retrieved to this point use a standard config that issues an error :)
			else
				fn:doc("configuration.xml")
		(: case 3 - full configuration sent with the configuration parameter - form style POST request :)
		else util:parse($submittedConfiguration)

	return $setConfiguration
};

(: gets and sets the return format :)
declare function xtriples:getFormat() {

	let $HEADER := request:get-header("format")
	let $GET := request:get-parameter("format", "rdf")
	let $setFormat :=
		if ($HEADER != "") then $HEADER
		else $GET

	return $setFormat
};



(: ########## SANITIZATION FUNCTIONS ################################################################ :)



(: safety filter for XPATH/XQuery expressions - dissallows executions from dangerous/not needed function namespaces :)
declare function xtriples:expressionSanityCheck($expression as xs:string) as xs:boolean {

	let $pattern := "((fn:.*\(.*\))|(doc*\(.*\))|(collection*\(.*\))|(v:.*\(.*\))|(backups:.*\(.*\))|(compression:.*\(.*\))|(contentextraction:.*\(.*\))|(counter:.*\(.*\))|(cqlparser:.*\(.*\))|(datetime:.*\(.*\))|(examples:.*\(.*\))|(exi:.*\(.*\))|(file:.*\(.*\))|(httpclient:.*\(.*\))|(image:.*\(.*\))|(inspection:.*\(.*\))|(jindi:.*\(.*\))|(kwic:.*\(.*\))|(lucene:.*\(.*\))|(mail:.*\(.*\))|(math:.*\(.*\))|(ngram:.*\(.*\))|(repo:.*\(.*\))|(request:.*\(.*\))|(response:.*\(.*\))|(scheduler:.*\(.*\))|(securitymanager:.*\(.*\))|(sequences:.*\(.*\))|(session:.*\(.*\))|(sort:.*\(.*\))|(sql:.*\(.*\))|(system:.*\(.*\))|(testing:.*\(.*\))|(text:.*\(.*\))|(transform:.*\(.*\))|(util:.*\(.*\))|(validation:.*\(.*\))|(xmldb:.*\(.*\))|(xmldiff:.*\(.*\))|(xqdoc:.*\(.*\))|(xslfo:.*\(.*\))|(config:.*\(.*\))|(docbook:.*\(.*\))|(app:.*\(.*\))|(dash:.*\(.*\))|(service:.*\(.*\))|(login-helper:.*\(.*\))|(packages:.*\(.*\))|(service:.*\(.*\))|(usermanager:.*\(.*\))|(demo:.*\(.*\))|(cex:.*\(.*\))|(ex:.*\(.*\))|(apputil:.*\(.*\))|(site:.*\(.*\))|(pretty:.*\(.*\))|(date:.*\(.*\))|(tei2:.*\(.*\))|(dbutil:.*\(.*\))|(docs:.*\(.*\))|(dq:.*\(.*\))|(review:.*\(.*\))|(epub:.*\(.*\))|(l18n:.*\(.*\))|(intl:.*\(.*\))|(restxq:.*\(.*\))|(tmpl:.*\(.*\))|(templates:.*\(.*\))|(trigger:.*\(.*\))|(jsjson:.*\(.*\))|(xqdoc:.*\(.*\)))"
	let $check := matches($expression, $pattern)

	return (not($check))
};



(: ########## EXPRESSION EVALUATION FUNCTIONS ######################################################## :)



(: evaluates expressions in curly braces within URI strings :)
declare function xtriples:expressionBasedUriResolver($uri as xs:string, $currentResource as node(), $repeatIndex as xs:integer) as xs:string {

	let $expression := concat('$currentResource', substring-after(substring-before($uri, "}"), "{"))
	let $finalExpression := 
		if (matches($expression, "\$repeatIndex")) then replace($expression, "\$repeatIndex", $repeatIndex) 
		else $expression

	let $result := 
		if (xtriples:expressionSanityCheck($finalExpression) = true()) then
			try { 
				util:eval($finalExpression) 
			} catch * { $err:description } 
		else ""

	let $uriWithSubstitution := if ($result) then replace($uri, "\{.*\}", $result) else ""

	return $uriWithSubstitution
}; 

(: evaluates expressions in curly braces within attribute values (prepend, append, repeat) :)
declare function xtriples:expressionBasedAttributeResolver($currentResource as node(), $attributeValue as xs:string*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as xs:string* {

	let $expression := substring-after(substring-before($attributeValue, "}"), "{")
	let $expressionSubstitution := 
		if (matches($expression, "\$resourceIndex")) then replace($expression, "\$resourceIndex", $resourceIndex) 
		else $expression
	let $finalExpression := 
		if (matches($expressionSubstitution, "\$repeatIndex")) then replace($expressionSubstitution, "\$repeatIndex", $repeatIndex) 
		else $expressionSubstitution

	let $result := 
		if ($finalExpression and xtriples:expressionSanityCheck($finalExpression)) then 
			try {
				replace($attributeValue, "\{.*\}", util:eval(concat("$currentResource", $finalExpression))) 
			} catch * { $err:description }
		else $attributeValue

	return $result
};

(: evaluates expressions in curly braces within a document string and retrieves the document :)
declare function xtriples:expressionBasedResourceResolver($collection as node()*, $resource as node()*) as item()* {

	let $collectionContent := if (fn:doc-available($collection/@uri)) then fn:doc($collection/@uri) else ""

	let $resourcesURI          := string($resource/@uri)
	let $resourcesExpression   := concat('$collectionContent', substring-after(substring-before($resourcesURI, "}"), "{"))
	let $resourcesNodes        := if (xtriples:expressionSanityCheck($resourcesExpression) = true()) then 
		try { util:eval($resourcesExpression) } catch * { $err:description } 
		else $resourcesExpression

	let $resources := 
		for $resource at $index in $resourcesNodes
			return 
				if ($resource instance of element()) then
					$resource
				else
					element {"resource"} {
						attribute {"uri"} {replace($resourcesURI, "\{.*\}", $resource)}
					}
	return $resources
};



(: ########## RESOURCE / COLLECTION FUNCTIONS ############################################################# :)



(: gets all resources for a collection, possibly expression based :)
declare function xtriples:getResources($collection as node()*) as item()* {

	let $resources := 
		for $resource in $collection/resource
		return
			if (matches($resource/@uri, "\{.*\}")) then 
				xtriples:expressionBasedResourceResolver($collection, $resource)
			else $resource

	return $resources
};



(: ########## EXTRACTION FUNCTIONS ######################################################################### :)



(:
Separate relatively similar functions are used for subject, predicate and object extraction.
This keeps the query simpler even if it repeats some code. At the same time it opens up the possibility 
to treat statement part extractions different in future versions (as it is already the case with subject/object 
vs predicate extraction routines.
:)

(: extracts subject statements from the current resource :)
declare function xtriples:extractSubjects($currentResource as node(), $statement as node()*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as item()* {

	let $externalResource := 
		if (exists($statement/subject/@resource)) then
			if (fn:doc-available(xtriples:expressionBasedUriResolver($statement/subject/@resource, $currentResource, $repeatIndex))) then 
				fn:doc(xtriples:expressionBasedUriResolver($statement/subject/@resource, $currentResource, $repeatIndex)) 
			else ""
		else ""

    let $debug1 :=  if ($statement/subject/@resource) then 
					local:log (
								"extract.xql" ||
								(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
								": xtriples:extractSubjects: external resource: '" ||
								$statement/subject/@resource/string() ||
								"' gives " ||
								count($externalResource) ||
								" node(s): " ||
								substring(serialize($externalResource), 1, 100) ||
								"...",
								(if ($statement/subject/@debug) then "info" else "trace")
							  )
				else ()

	let $subjectExpressionConcatenation := 
		if ($externalResource) then concat("$externalResource", string($statement/subject))
		else concat('$currentResource', string($statement/subject))

	let $subjectExpression := 
		if (matches($subjectExpressionConcatenation, "\$repeatIndex")) then
			replace($subjectExpressionConcatenation, "\$repeatIndex", $repeatIndex)
		else
			$subjectExpressionConcatenation

           let $debug2 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
							": xtriples:extractSubjects: before util:eval('" ||
							$subjectExpression ||
							"') ...",
							(if ($statement/subject/@debug) then "info" else "trace")
						  )

	let $subjectNodes :=	if (xtriples:expressionSanityCheck($subjectExpression)) then 
								try {
										util:eval($subjectExpression)
									}
								catch * { $err:description } 
							else $subjectExpression

    let $debug3 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
							": xtriples:extractSubjects: ... evaluated to " ||
							count($subjectNodes) ||
							" subjects.",
							(if ($statement/subject/@debug) then "info" else "trace")
						  )

	let $subjects := 
		for $subjectValue in $subjectNodes
		return functx:copy-attributes(<subject>{string($subjectValue)}</subject>, $statement/subject)

	return $subjects
};

(: extracts predicate statements from the current resource :)
declare function xtriples:extractPredicate($currentResource as node(), $statement as node()*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as item()* {

	let $externalResource := 
		if (exists($statement/predicate/@resource)) then
			if (fn:doc-available(xtriples:expressionBasedUriResolver($statement/predicate/@resource, $currentResource, $repeatIndex))) then 
				fn:doc(xtriples:expressionBasedUriResolver($statement/predicate/@resource, $currentResource, $repeatIndex)) 
			else ""
		else ""

    let $debug1 :=  if ($statement/predicate/@resource) then 
					local:log (
								"extract.xql" ||
								(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
								": xtriples:extractPredicate: external resource: '" ||
								$statement/predicate/@resource/string() ||
								"' gives " ||
								count($externalResource) ||
								" node(s): " ||
								substring(serialize($externalResource), 1, 100) ||
								"...",
								(if ($statement/predicate/@debug) then "info" else "trace")
							  )
				else ()

	let $predicateExpressionConcatenation := 
		if ($externalResource) then concat("$externalResource", string($statement/predicate))
		else concat('$currentResource', string($statement/predicate))

	let $predicateExpression := 
		if (matches($predicateExpressionConcatenation, "\$repeatIndex")) then
			replace($predicateExpressionConcatenation, "\$repeatIndex", $repeatIndex)
		else
			$predicateExpressionConcatenation

    let $debug2 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
							": xtriples:extractPredicate: before util:eval('" ||
							$predicateExpression ||
							"') ...",
							(if ($statement/predicate/@debug) then "info" else "trace")
						  )

	let $predicateValue := if (xtriples:expressionSanityCheck($predicateExpression)) then 
		                      try { string(util:eval($predicateExpression)) } catch * { $err:description } 
    		                else $predicateExpression

    let $debug3 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
							": xtriples:extractPredicate: ... evaluated to '" ||
							$predicateValue ||
							"'.",
							(if ($statement/predicate/@debug) then "info" else "trace")
						  )

	let $predicate := <predicate prefix="{$statement/predicate/@prefix}">{string($predicateValue)}</predicate>

	return $predicate
};

(: extracts object statements from the current resource :)
declare function xtriples:extractObjects($currentResource as node(), $statement as node()*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as item()* {

	let $externalResource := 
        if ($statement/object/@resource) then
	       if (fn:doc-available(xtriples:expressionBasedUriResolver($statement/object/@resource, $currentResource, $repeatIndex))) then 
		      fn:doc(xtriples:expressionBasedUriResolver($statement/object/@resource, $currentResource, $repeatIndex)) 
           else ()
	    else ()

    let $debug1 :=  if ($statement/object/@resource) then 
					       local:log (
								"extract.xql" ||
								(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
								": xtriples:extractObjects: external resource: '" ||
								$statement/object/@resource/string() ||
								"' gives " ||
								count($externalResource) ||
								" node(s): " ||
								substring(serialize($externalResource), 1, 100) ||
								"...",
								(if ($statement/object/@debug) then "info" else "trace")
							  )
				else ()

	let $objectExpressionConcatenation := 
	   if (starts-with(string($statement/object), '/') and $externalResource) then
			concat("$externalResource", string($statement/object))
	   else if (starts-with(string($statement/object), '/')) then
			concat("$currentResource", string($statement/object))
	   else
		   string($statement/object)

	let $objectExpression := replace($objectExpressionConcatenation, "\$repeatIndex", $repeatIndex)

    let $debug2 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
							": xtriples:extractObjects: before util:eval('" ||
							$objectExpression ||
							"') ...",
							(if ($statement/object/@debug) then "info" else "trace")
						  )

	let $objectNodes := if (xtriples:expressionSanityCheck($objectExpression)) then 
							try { 
									util:eval($objectExpression)
								}
							catch * { $err:description }
						else $objectExpression

    let $debug3 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
							": xtriples:extractObjects: ... evaluated to " ||
							count($objectNodes) ||
							" objects.",
							(if ($statement/object/@debug) then "info" else "trace")
						  )

	let $objects :=
		for $objectValue in $objectNodes
		  return functx:copy-attributes(<object>{string($objectValue)}</object>, $statement/object)

	return $objects
};

(: XTriples core function - evaluates all configured statements for the current resource :)
declare function xtriples:extractTriples($currentResource as node(), $resourceIndex as xs:integer, $configuration as node()*) as item()* {

	(: set the content of the current resource :)
	let $currentResource := 
		(: it can be a resource tag with an URI :)
		if (xs:anyURI($currentResource/@uri) and fn:doc-available($currentResource/@uri)) then 
			fn:doc($currentResource/@uri)
		(: or a resource tag with children :)
		else if ($currentResource/*) then 
			$currentResource/*
		else fn:doc("errors.xml")//error[@type='resource_no_content']

	(: start statement pattern extraction :)
	for $statement in $configuration//triples/*

		let $repeat := 
			if ($statement/@repeat) then
				xs:integer(xtriples:expressionBasedAttributeResolver($currentResource, $statement/@repeat, 1, $resourceIndex))
			else 1

    let $debug1 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, ')') else ()) ||
							": xtriples:extractTriples $repeat=" ||
							$repeat ||
							".",
							(if ($statement//@debug) then "info" else "trace")
						  )

		for $repeatIndex in (1 to $repeat)

			(: if a condition expression is given in the current statement, evaluate it :)
			let $condition := 
				if (exists($statement/condition)) then 
					if (xtriples:expressionSanityCheck(string($statement/condition)) = true()) then
						try { boolean(util:eval(concat('$currentResource', string($statement/condition)))) } catch * { $err:description }
					else true()
				else true()

			(: n possible subjects per statement declaration :)
			let $subjects := 
				if (starts-with(string($statement/subject), '/')) then 
					xtriples:extractSubjects($currentResource, $statement, $repeatIndex, $resourceIndex) 
				else $statement/subject

			(: 1 predicate per statement declaration :)
			let $predicate := 
				if (starts-with(string($statement/predicate), '/')) then 
					xtriples:extractPredicate($currentResource, $statement, $repeatIndex, $resourceIndex) 
				else <predicate prefix="{$statement/predicate/@prefix}">{string($statement/predicate)}</predicate>

			(: n possible objects per statement declaration :)
			let $objects := 
				if (starts-with(string($statement/object), '/')) then 
					xtriples:extractObjects($currentResource, $statement, $repeatIndex, $resourceIndex) 
				else $statement/object

            let $debug2 :=  local:log (
							"extract.xql" ||
							(if ($statement/@n) then concat(' (', $statement/@n, '/', $repeatIndex, ')') else ()) ||
							": xtriples:extractTriples has " ||
							count($subjects) ||
							" subjects, predicate " ||
							string($predicate) ||
							" and " ||
							count($objects) ||
							" objects.",
							(if ($statement//@debug) then "info" else "trace")
						  )


			(: build statements - but only if the condition expression returned any value - boolean, string, node set etc. :)
			let $statements := 
				if ($condition) then 
					for $subject in $subjects
						let $subjectReturn :=
							if ($subject = "") then "" else
							for $object in $objects

								let $subjectPrepend := xtriples:expressionBasedAttributeResolver($currentResource, $subject/@prepend, $repeatIndex, $resourceIndex)
								let $subjectAppend  := xtriples:expressionBasedAttributeResolver($currentResource, $subject/@append, $repeatIndex, $resourceIndex)

								let $predicatePrepend := xtriples:expressionBasedAttributeResolver($currentResource, $predicate/@prepend, $repeatIndex, $resourceIndex)
								let $predicateAppend  := xtriples:expressionBasedAttributeResolver($currentResource, $predicate/@append, $repeatIndex, $resourceIndex)

								let $objectPrepend := xtriples:expressionBasedAttributeResolver($currentResource, $object/@prepend, $repeatIndex, $resourceIndex)
								let $objectAppend  := xtriples:expressionBasedAttributeResolver($currentResource, $object/@append, $repeatIndex, $resourceIndex)

								let $objectReturn := 
									if ($object = "") then "" 
									else <statement>{(
										functx:remove-attributes(functx:copy-attributes(<subject>{concat($subjectPrepend, $subject, $subjectAppend)}</subject>, $subject), ('append', 'prepend')),
										functx:remove-attributes(functx:copy-attributes(<predicate>{concat($predicatePrepend, $predicate, $predicateAppend)}</predicate>, $predicate), ('append', 'prepend')),
										functx:remove-attributes(functx:copy-attributes(<object>{concat($objectPrepend, $object, $objectAppend)}</object>, $object), ('append', 'prepend'))
									)}</statement>
							return $objectReturn
					return $subjectReturn
				else ""

	return $statements
};



(: ########## FORMAT FUNCTIONS #################################################################################### :)



(: internal/intermediate XTriples RDF format, one RDF node per XTriples statement - will later be condensed by the any23 webservice to "real" RDF :)
declare function xtriples:generateRDFTriples($xtriples as node()*) as item()* {

	for $statement in $xtriples//statements/statement

		let $subjectType := $statement/subject/@type
		let $subjectPrefix := $statement/subject/@prefix
		let $subjectUri := $xtriples//vocabularies/vocabulary[@prefix=$subjectPrefix]/@uri
		let $subjectValue := $statement/subject

		let $predicatePrefix := $statement/predicate/@prefix
		let $predicateUri := $xtriples//vocabularies/vocabulary[@prefix=$predicatePrefix]/@uri
		let $predicateValue := $statement/predicate

		let $objectType := $statement/object/@type
		let $objectPrefix := $statement/object/@prefix
		let $objectUri := $xtriples//vocabularies/vocabulary[@prefix=$objectPrefix]/@uri
		let $objectValue := $statement/object/text()

		(: rdf triple construction using computed constructors with qualified names; builds single rdf triples which are later combined by any23 webservice :)
		let $triple := 
			(: outer RDF tag :)
			element {QName("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf:Description")} {
			(: either is a blank node, then gets rdf:nodeID attribute :)
			(if ($subjectType = 'bnode') then 
				attribute {QName("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf:nodeID")} { $subjectValue }
			(: or is a URI, then gets rdf:about attribute :)
			else
				attribute {QName("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf:about")} { concat($subjectUri, $subjectValue) }),
				(: predicate = inner element of the RDF tag :)
				element {QName($predicateUri, concat($predicatePrefix, ":", $predicateValue))} {
				(: object uri; set rdf:resource as attribute to predicate :)
				if ($objectType = 'uri') then 
					attribute {QName("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf:resource")} { 
						if ($objectPrefix) then concat($objectUri, $objectValue) else $objectValue 
					}
				(: object blank node; set rdf:nodeIFD as attribute to predicate :)
				else if ($objectType = 'bnode') then
					attribute {QName("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf:nodeID")} { $objectValue }
				(: typed object literals; append rdf:datatype as attribute and set value as text node :)
				else if ($statement/object/@datatype) then
					(attribute {QName("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf:datatype")} { concat("http://www.w3.org/2001/XMLSchema#", $statement/object/@datatype) }, $objectValue)
				(: language tagged object literals; append rdf:datatype as attribute and set value as text node :)
				else if ($statement/object/@lang) then
					(attribute {"xml:lang"} { $statement/object/@lang }, $objectValue)
				(: plain object literals; set value as text node :)
				else
					$objectValue
			}
		}

		return
			$triple
};

(: gets the internal RDF format :)
declare function xtriples:getRDFTriples($xtriples as node()*, $vocabularies as node()*) as item()* {
	let $rdfTriples := <rdftriples>{$vocabularies}{xtriples:generateRDFTriples($xtriples)}</rdftriples>
	return $rdfTriples
};

(: gets the internal RDF format and sends it to any23 for further transformation :)
declare function xtriples:getRDF($xtriples as node()*, $vocabularies as node()*) as item()* {
	(: internal RDF format :)
	let $rdfstylesheet := doc("rdf.xsl")
	let $rdfTriples := xtriples:getRDFTriples($xtriples, $vocabularies)
	let $rdfInternal := transform:transform($rdfTriples, $rdfstylesheet, ())

	(: official RDF format via any23 :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($config:any23WebserviceURL, "rdfxml")), $rdfInternal, false(), $headers)
	let $rdf := $POST_request//httpclient:body/*

	return $rdf
};

(: gets ntriples format from any23 by sending in extracted RDF :)
declare function xtriples:getNTRIPLES($rdf as node()*) as item()* {

	(: url encoded ntriples :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($config:any23WebserviceURL, "nt")), $rdf, false(), $headers)
	let $ntriples := util:unescape-uri(replace(string($POST_request//httpclient:body), '%00', ''), "UTF-8")

	return $ntriples
};

(: gets turtle format from any23 by sending in extracted RDF :)
declare function xtriples:getTURTLE($rdf as node()*) as item()* {

	(: Since at least 3.0rc2, eXist does return text instead of base64Binary :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($config:any23WebserviceURL, "turtle")), $rdf, false(), $headers)
(:	let $turtle := util:binary-to-string(xs:base64Binary($POST_request//httpclient:body), "UTF-8"):)
    let $turtle := xmldb:decode($POST_request//httpclient:body)
	return $turtle
};

(: gets nquads format from any23 by sending in extracted RDF :)
declare function xtriples:getNQUADS($rdf as node()*) as item()* {

	(: eXist returns base64Binary nquads :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($config:any23WebserviceURL, "nq")), $rdf, false(), $headers)
	let $nquads := util:binary-to-string(xs:base64Binary($POST_request//httpclient:body), "UTF-8")

	return $nquads
};

(: gets json format from any23 by sending in extracted RDF :)
declare function xtriples:getJSON($rdf as node()*) as item()* {

	(: eXist returns base64Binary json :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($config:any23WebserviceURL, "json")), $rdf, false(), $headers)
	let $json := util:binary-to-string(xs:base64Binary($POST_request//httpclient:body), "UTF-8")

	return $json
};

(: gets trix format from any23 by sending in extracted RDF :)
declare function xtriples:getTRIX($rdf as node()*) as item()* {

	(: eXist returns base64Binary json :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($config:any23WebserviceURL, "trix")), $rdf, false(), $headers)
	let $trix := $POST_request//httpclient:body/*

	return $trix
};

(: gets svg format from rhizomik webservice by temporarily storing the extracted RDF and pointing the redefer to the temporary file :)
declare function xtriples:getSVG($rdf as node()*) as item()* {

	(: svg format with temporary file :)
	let $filename := concat(util:uuid(), ".xml")
	let $store := xmldb:store($config:app-root || "/temp/", $filename, $rdf)
	let $svgHeaders := 
		<headers>
			<header name="format" value="RDF/XML"/>
			<header name="mode" value="svg" />
			<headers name="rules" value="{$config:redeferWebserviceRulesURL}" />
		</headers>
	
	let $GET_request := httpclient:get(xs:anyURI(concat($config:redeferWebserviceURL, "render?rdf=", $config:xtriplesWebserviceURL, "temp/", $filename, "&amp;format=RDF/XML&amp;mode=svg&amp;rules=", $config:redeferWebserviceRulesURL)), false(), $svgHeaders)
	let $svg := $GET_request//httpclient:body/*
	let $delete := xmldb:remove($config:app-root || "/temp/", $filename)

	return $svg
};



(: ########## MAIN QUERY BODY ############################################################################################### :)


let $debug1 :=  local:log ("extract.xql: Beginning processing...", "info")

(: set basic vars :)
let $collections := $setConfiguration/xtriples/collection
let $configuration := $setConfiguration/xtriples/configuration
let $vocabularies := $configuration/vocabularies

(: dynamic namespace declaration for all configured vocabularies :)
let $namespaces := 
	if ($configuration/vocabularies/*) then
		for $namespace in $configuration/vocabularies/*
		return util:declare-namespace($namespace/@prefix, $namespace/@uri)
	else ""

(: extract triples from collections :)
let $extraction := 

	for $collection in $collections

		let $maxResources :=
			if ($collection/@max > 0) then
				$collection/@max
			else 0

		let $resources := xtriples:getResources($collection)

		let $triples :=
			for $resource at $resourceIndex in $resources
				let $currentResource :=
					if (name($resource) = 'resource') then
						$resource
					else <resource>{$resource}</resource>
			return
				if ($maxResources > 0 and $resourceIndex <= $maxResources) 
				then
					<statements>{xtriples:extractTriples($currentResource, $resourceIndex, $configuration)}</statements>
				else if ($maxResources = 0)
				then
					<statements>{xtriples:extractTriples($currentResource, $resourceIndex, $configuration)}</statements>
				else ""

	return <result>{(functx:copy-attributes(<collection>{$resources}</collection>, $collection),<triples>{$triples}</triples>)}</result>

(: construct internal result format :)
let $xtriples := 
	<xtriples>
		<configuration>
			{$vocabularies}
			<triples>{$extraction//triples/*}</triples>
		</configuration>
		{$extraction//collection}
	</xtriples>

(: transform and return result :)
return (
	if ($setFormat = "xtriples") then
		$xtriples
	else if ($setFormat = "rdftriples") then

       let $debug1 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )

		return xtriples:getRDFTriples($xtriples, $vocabularies)
	else if ($setFormat = "ntriples") then (

       let $debug2 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )

		return    response:set-header("Content-Type", "text/plain; charset=UTF-8"),
		          response:stream(xtriples:getNTRIPLES(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "turtle") then (

       let $debug3 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )

		return    response:set-header("Content-Type", "text/turtle; charset=UTF-8"),
		          response:stream(xtriples:getTURTLE(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "nquads") then (

       let $debug4 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )

		return    response:set-header("Content-Type", "text/x-nquads; charset=UTF-8"),
                 response:stream(xtriples:getNQUADS(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "json") then (

       let $debug5 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )

		return    response:set-header("Content-Type", "text/json; charset=UTF-8"),
		          response:stream(xtriples:getJSON(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "trix") then (

       let $debug6 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )

		return    response:set-header("Content-Type", "application/trix; charset=UTF-8"),
		          xtriples:getTRIX(xtriples:getRDF($xtriples, $vocabularies))
		)
	else if ($setFormat = "svg") then
		(: response:set-header("Content-Type", "image/svg+xml; charset=UTF-8"), :)

       let $debug7 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )

		return    xtriples:getSVG(xtriples:getRDF($xtriples, $vocabularies))
	else 
       let $debug8 :=  local:log (
							"extract.xql: output in " || $setFormat  || ".",
							"trace"
						  )


		return    xtriples:getRDF($xtriples, $vocabularies)
)
