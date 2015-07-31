xquery version "3.0";

(:~
 : XTriples
 :
 : A generic webservice to extract rdf statements from XML resources
 :
 : @author Torsten Schrade
 : @email <Torsten.Schrade@adwmainz.de>
 : @version 1.2.0 
 : @licence MIT
 :
 : Main module containing the webservice
:)

(: ### PROLOGUE ### :)

import module namespace functx="http://www.functx.com";

declare namespace xtriples = "http://xtriples.spatialhumanities.de/";

(: ### GLOBAL VARIABLES :)

declare variable $setConfiguration := xtriples:getConfiguration();
declare variable $setFormat := xtriples:getFormat();
declare variable $xtriplesWebserviceURL := "http://xtriples.spatialhumanities.de/";
declare variable $any23WebserviceURL := "https://any23.apache.org/";
declare variable $redeferWebserviceURL := "http://rhizomik.net/redefer-services/";
declare variable $redeferWebserviceRulesURL := "http://rhizomik.net:8080/html/redefer/rdf2svg/showgraph.jrule";

(: ### CONFIGURATION FUNCTIONS ### :)

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

(: ### CRAWLING FUNCTIONS ### :)

(: evaluates an XPATH expression in curly braces within a (URI) string and returns the string :)
declare function xtriples:expressionBasedUriResolver($uri as xs:string, $currentResource as node()) as xs:string {

	let $expression := concat('$currentResource', substring-after(substring-before($uri, "}"), "{"))
	let $result := if (xtriples:expressionSanityCheck($expression) = true()) then 
		try { util:eval($expression) } catch * { $err:description } 
		else ""
	let $uriWithSubstitution := replace($uri, "\{.*\}", $result)

	return $uriWithSubstitution
};

(: evaluates a possible XPATH expression in curly braces within a document string and retrieves the document :)
declare function xtriples:expressionBasedResourceResolver($collection as node()*, $resource as node()*) as item()* {

	let $collectionContent := if (fn:doc-available($collection/@uri)) then fn:doc($collection/@uri) else ""

	let $resourcesURI := string($resource/@uri)
	let $resourcesExpression := concat('$collectionContent', substring-after(substring-before($resourcesURI, "}"), "{"))
	let $resourcesNodes := if (xtriples:expressionSanityCheck($resourcesExpression) = true()) then 
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

(: gets resources from a collection, possibly expression based :)
declare function xtriples:getResources($collection as node()*) as item()* {

	let $resources := 
		for $resource in $collection/resource
		return
			if (matches($resource/@uri, "\{") and matches($resource/@uri, "\}")) then 
				xtriples:expressionBasedResourceResolver($collection, $resource)
			else $resource

	return $resources
};

(: ### EXTRACTION FUNCTIONS ### :)

(:
Separate relatively similar functions are used for subject, predicate and object extraction.
This keeps the query simpler even if it repeats some code. At the same time it opens up the possibility 
to treat statement part extractions different in future versions (as it is already the case with subject/object 
vs predicate extraction routines.
:)

(: safety filter for XPATH/XQuery expressions - dissallows executions from dangerous/not needed function namespaces :)
declare function xtriples:expressionSanityCheck($expression as xs:string) as xs:boolean {

	let $pattern := "((fn:.*\(.*\))|(doc*\(.*\))|(collection*\(.*\))|(v:.*\(.*\))|(backups:.*\(.*\))|(compression:.*\(.*\))|(contentextraction:.*\(.*\))|(counter:.*\(.*\))|(cqlparser:.*\(.*\))|(datetime:.*\(.*\))|(examples:.*\(.*\))|(exi:.*\(.*\))|(file:.*\(.*\))|(httpclient:.*\(.*\))|(image:.*\(.*\))|(inspection:.*\(.*\))|(jindi:.*\(.*\))|(kwic:.*\(.*\))|(lucene:.*\(.*\))|(mail:.*\(.*\))|(math:.*\(.*\))|(ngram:.*\(.*\))|(repo:.*\(.*\))|(request:.*\(.*\))|(response:.*\(.*\))|(scheduler:.*\(.*\))|(securitymanager:.*\(.*\))|(sequences:.*\(.*\))|(session:.*\(.*\))|(sort:.*\(.*\))|(sql:.*\(.*\))|(system:.*\(.*\))|(testing:.*\(.*\))|(text:.*\(.*\))|(transform:.*\(.*\))|(util:.*\(.*\))|(validation:.*\(.*\))|(xmldb:.*\(.*\))|(xmldiff:.*\(.*\))|(xqdoc:.*\(.*\))|(xslfo:.*\(.*\))|(config:.*\(.*\))|(docbook:.*\(.*\))|(app:.*\(.*\))|(dash:.*\(.*\))|(service:.*\(.*\))|(login-helper:.*\(.*\))|(packages:.*\(.*\))|(service:.*\(.*\))|(usermanager:.*\(.*\))|(demo:.*\(.*\))|(cex:.*\(.*\))|(ex:.*\(.*\))|(apputil:.*\(.*\))|(site:.*\(.*\))|(pretty:.*\(.*\))|(date:.*\(.*\))|(tei2:.*\(.*\))|(dbutil:.*\(.*\))|(docs:.*\(.*\))|(dq:.*\(.*\))|(review:.*\(.*\))|(epub:.*\(.*\))|(l18n:.*\(.*\))|(intl:.*\(.*\))|(restxq:.*\(.*\))|(tmpl:.*\(.*\))|(templates:.*\(.*\))|(trigger:.*\(.*\))|(jsjson:.*\(.*\))|(xqdoc:.*\(.*\)))"
	let $check := matches($expression, $pattern)

	return if ($check = true()) then false() else true()
};

(: extracts subject statements from the current resource :)
declare function xtriples:extractSubjects($currentResource as node(), $statement as node()*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as item()* {

	let $externalResource := 
		if (exists($statement/subject/@resource)) then
			if (fn:doc-available(xtriples:expressionBasedUriResolver($statement/subject/@resource, $currentResource))) then 
				fn:doc(xtriples:expressionBasedUriResolver($statement/subject/@resource, $currentResource)) 
			else ""
		else ""

	let $subjectExpressionConcatenation := 
		if ($externalResource) then concat("$externalResource", string($statement/subject))
		else concat('$currentResource', string($statement/subject))

	let $subjectExpression := 
		if (matches($subjectExpressionConcatenation, "$repeatIndex")) then replace($subjectExpressionConcatenation, "$repeatIndex", $repeatIndex)
		else $subjectExpressionConcatenation

	let $subjectNodes := if (xtriples:expressionSanityCheck($subjectExpression) = true()) then 
		try { util:eval($subjectExpression) } catch * { $err:description } 
		else $subjectExpression

	let $subjects := 
		for $subjectValue in $subjectNodes
		return functx:copy-attributes(<subject>{string($subjectValue)}</subject>, $statement/subject)

	return $subjects
};

(: extracts predicate statements from the current resource :)
declare function xtriples:extractPredicate($currentResource as node(), $statement as node()*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as item()* {

	let $externalResource := 
		if (exists($statement/predicate/@resource)) then
			if (fn:doc-available(xtriples:expressionBasedUriResolver($statement/predicate/@resource, $currentResource))) then 
				fn:doc(xtriples:expressionBasedUriResolver($statement/predicate/@resource, $currentResource)) 
			else ""
		else ""

	let $predicateExpressionConcatenation := 
		if ($externalResource) then concat("$externalResource", string($statement/predicate))
		else concat('$currentResource', string($statement/predicate))

	let $predicateExpression := 
		if (matches($predicateExpressionConcatenation, "$repeatIndex")) then replace($predicateExpressionConcatenation, "$repeatIndex", $repeatIndex)
		else $predicateExpressionConcatenation

	let $predicateValue := if (xtriples:expressionSanityCheck($predicateExpression) = true()) then 
		try { string(util:eval($predicateExpression)) } catch * { $err:description } 
		else $predicateExpression

	let $predicate := <predicate prefix="{$statement/predicate/@prefix}">{string($predicateValue)}</predicate>

	return $predicate
};

(: extracts object statements from the current resource :)
declare function xtriples:extractObjects($currentResource as node(), $statement as node()*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as item()* {

	let $externalResource := 
		if (exists($statement/object/@resource)) then
			if (fn:doc-available(xtriples:expressionBasedUriResolver($statement/object/@resource, $currentResource))) then 
				fn:doc(xtriples:expressionBasedUriResolver($statement/object/@resource, $currentResource)) 
			else ""
		else ""

	let $objectExpressionConcatenation := 
		if ($externalResource) then concat("$externalResource", string($statement/object))
		else concat("$currentResource", string($statement/object))

	let $objectExpression := 
		if (matches($objectExpressionConcatenation, "$repeatIndex")) then replace($objectExpressionConcatenation, "$repeatIndex", $repeatIndex)
		else $objectExpressionConcatenation

	let $objectNodes := if (xtriples:expressionSanityCheck($objectExpression) = true()) then 
		try { util:eval($objectExpression) } catch * { $err:description }
		else $objectExpression

	let $objects :=
		for $objectValue in $objectNodes
		return functx:copy-attributes(<object>{string($objectValue)}</object>, $statement/object)

	return $objects
};

(: extracts expressions in prepend/append attributes from the current resource :)
declare function xtriples:extractPrependAppend($currentResource as node(), $expressionString as xs:string*, $repeatIndex as xs:integer, $resourceIndex as xs:integer) as xs:string* {
	let $expression := substring-after(substring-before($expressionString, "}"), "{")
	let $result := 
		if ($expression and xtriples:expressionSanityCheck($expression) = true()) then 
			try {
				replace($expressionString, "\{.*\}", util:eval(concat("$currentResource", $expression))) 
			} catch * { $err:description }
		else $expressionString
	return $result
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

		let $repeat := if ($statement/@repeat > 0) then $statement/@repeat else 1

		for $repeatIndex in (1 to $repeat)

			(: if a condition expression is given in the current statement, evaluate it :)
			let $condition := 
				if (exists($statement/condition)) then 
					if (xtriples:expressionSanityCheck(string($statement/condition)) = true()) then
						try { util:eval(concat('$currentResource', string($statement/condition))) } catch * { $err:description }
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

			(: build statements - but only if the condition expression returned any value - boolean, string, node set etc. :)
			let $statements := 
				if ($condition != false()) then 
					for $subject in $subjects
						let $subjectReturn :=
							if ($subject = "") then "" else
							for $object in $objects

								let $subjectPrepend := xtriples:extractPrependAppend($currentResource, $subject/@prepend, $repeatIndex, $resourceIndex)
								let $subjectAppend := xtriples:extractPrependAppend($currentResource, $subject/@append, $repeatIndex, $resourceIndex)

								let $predicatePrepend := xtriples:extractPrependAppend($currentResource, $predicate/@prepend, $repeatIndex, $resourceIndex)
								let $predicateAppend := xtriples:extractPrependAppend($currentResource, $predicate/@append, $repeatIndex, $resourceIndex)

								let $objectPrepend := xtriples:extractPrependAppend($currentResource, $object/@prepend, $repeatIndex, $resourceIndex)
								let $objectAppend := xtriples:extractPrependAppend($currentResource, $object/@append, $repeatIndex, $resourceIndex)

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

(: ### FORMAT FUNCTIONS ### :)

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
	let $POST_request := httpclient:post(xs:anyURI(concat($any23WebserviceURL, "rdfxml")), $rdfInternal, false(), $headers)
	let $rdf := $POST_request//httpclient:body/*

	return $rdf
};

(: gets ntriples format from any23 by sending in extracted RDF :)
declare function xtriples:getNTRIPLES($rdf as node()*) as item()* {

	(: url encoded ntriples :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($any23WebserviceURL, "nt")), $rdf, false(), $headers)
	let $ntriples := util:unescape-uri(replace(string($POST_request//httpclient:body), '%00', ''), "UTF-8")

	return $ntriples
};

(: gets turtle format from any23 by sending in extracted RDF :)
declare function xtriples:getTURTLE($rdf as node()*) as item()* {

	(: eXist returns base64Binary turtle :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($any23WebserviceURL, "turtle")), $rdf, false(), $headers)
	let $turtle := util:binary-to-string(xs:base64Binary($POST_request//httpclient:body), "UTF-8")

	return $turtle
};

(: gets nquads format from any23 by sending in extracted RDF :)
declare function xtriples:getNQUADS($rdf as node()*) as item()* {

	(: eXist returns base64Binary nquads :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($any23WebserviceURL, "nq")), $rdf, false(), $headers)
	let $nquads := util:binary-to-string(xs:base64Binary($POST_request//httpclient:body), "UTF-8")

	return $nquads
};

(: gets json format from any23 by sending in extracted RDF :)
declare function xtriples:getJSON($rdf as node()*) as item()* {

	(: eXist returns base64Binary json :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($any23WebserviceURL, "json")), $rdf, false(), $headers)
	let $json := util:binary-to-string(xs:base64Binary($POST_request//httpclient:body), "UTF-8")

	return $json
};

(: gets trix format from any23 by sending in extracted RDF :)
declare function xtriples:getTRIX($rdf as node()*) as item()* {

	(: eXist returns base64Binary json :)
	let $headers := <headers><header name="Content-Type" value="application/rdf+xml; charset=UTF-8"/></headers>
	let $POST_request := httpclient:post(xs:anyURI(concat($any23WebserviceURL, "trix")), $rdf, false(), $headers)
	let $trix := $POST_request//httpclient:body/*

	return $trix
};

(: gets svg format from rhizomik webservice by temporarily storing the extracted RDF and pointing the redefer to the temporary file :)
declare function xtriples:getSVG($rdf as node()*) as item()* {

	(: svg format with temporary file :)
	let $filename := concat(util:uuid(), ".xml")
	let $store := xmldb:store("/db/apps/xtriples/temp/", $filename, $rdf)
	let $svgHeaders := 
		<headers>
			<header name="format" value="RDF/XML"/>
			<header name="mode" value="svg" />
			<headers name="rules" value="{$redeferWebserviceRulesURL}" />
		</headers>
	
	let $GET_request := httpclient:get(xs:anyURI(concat($redeferWebserviceURL, "render?rdf=", $xtriplesWebserviceURL, "temp/", $filename, "&amp;format=RDF/XML&amp;mode=svg&amp;rules=", $redeferWebserviceRulesURL)), false(), $svgHeaders)
	let $svg := $GET_request//httpclient:body/*
	let $delete := xmldb:remove("/db/apps/xtriples/temp/", $filename)

	return $svg
};

(: ### QUERY BODY ### :)

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
		xtriples:getRDFTriples($xtriples, $vocabularies)
	else if ($setFormat = "ntriples") then (
		response:set-header("Content-Type", "text/plain; charset=UTF-8"),
		response:stream(xtriples:getNTRIPLES(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "turtle") then (
		response:set-header("Content-Type", "text/turtle; charset=UTF-8"),
		response:stream(xtriples:getTURTLE(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "nquads") then (
		response:set-header("Content-Type", "text/x-nquads; charset=UTF-8"),
		response:stream(xtriples:getNQUADS(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "json") then (
		response:set-header("Content-Type", "text/json; charset=UTF-8"),
		response:stream(xtriples:getJSON(xtriples:getRDF($xtriples, $vocabularies)), "method=text")
		)
	else if ($setFormat = "trix") then (
		response:set-header("Content-Type", "application/trix; charset=UTF-8"),
		xtriples:getTRIX(xtriples:getRDF($xtriples, $vocabularies))
		)
	else if ($setFormat = "svg") then
		(: response:set-header("Content-Type", "image/svg+xml; charset=UTF-8"), :)
		xtriples:getSVG(xtriples:getRDF($xtriples, $vocabularies))
	else 
		(: response:set-header("Content-Type", "application/rdf+xml; charset=UTF-8"), :)
		xtriples:getRDF($xtriples, $vocabularies)
)