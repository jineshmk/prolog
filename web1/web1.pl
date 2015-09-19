% Simple JSON validator
% Author Jinesh
% Date 19/09/2015

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_mime_plugin)).
:- use_module(library(http/http_client)).


%---------Web part--------------------------------------
:- http_handler('/', index, []).
:- http_handler('/validate', validate_json,[]).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).


index(_Request) :-
	format('Content-type: text/html~n~n'),
        format('<html><head><title>Test</title></head><body><h2>JSON Validation</h2><form action=\'validate\' method=\'post\'> JSON Here:<input name=\'input_val\' type=text > <input type=submit> </input></input></form><p>Sample Input {key1:value1, key2:value2} (Not support nested JSON). </p></body></html>~n').
        
validate_json(_Request) :-
	member(method(post), _Request), !,
        http_read_data(_Request, [input_val=IN], []),
        format('Content-type: text/html~n~n', []),
        format('<html><head><title>Test</title></head><body>'),
        (jsonval(IN) -> format('<h1>Correct Syntax</h1>');format('<h1>Wrong Syntax</h1>')),
        format('<br/><a href=\'/\'>Back</a></body></html>~n').
        
