%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(nkadmin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export_type([update/0]).

%% ===================================================================
%% Types
%% ===================================================================


-type update() ::
    #{
        class => update_class(),        % Mandatory
        id => update_id(),              % Optional
        value => update_value()
    }.

-type update_id() :: binary().

-type update_class() ::
    frame |
    tree |
    detail |
    url |
    popup |
    breadcrumbs |
    frameDomainName | frameDomainIcon |
    frameUserIcon | frameUserIcon | frameUserMenu |
    menuCategory | menuGroup | menuEntry.



-type update_value() ::
    #{
        label => binary(),
        css => binary(),
        icon => binary(),
        tooltip => binary(),
        items => [update()] | [binary()]
    }.


%% // Confirmación de aviso/error que puede ser cancelada pulsando fuera
%% var popup = {
%%     class: "popup",
%%     value: {
%%         class: "webix_confirm", // tipo de popup
%%         value: {
%%             title: "Title",
%%             text: "Text",
%%             ok: "OK",
%%             cancel: "Cancel",
%%             //type: "confirm-warning",
%%             type: "confirm-error",
%%             callback: {
%%                 nkParseFunction:
%%                 'function(response) {   \
%%                     console.log("RESPONSE (true/false): ", response); \
%%                 }'
%%             }
%%         }
%%     }
%% }
%%
%% // Mensaje de información/error con un tiempo límite o que puede quedarse fija hasta ser clicada por el usuario
%% var popup = {
%%     class: "popup",
%%     value: {
%%         class: "webix_message", // tipo de popup
%%         value: {
%%             text: "Message text", // Mensaje de texto
%%             type: "error", // Color del fondo en rojo para error (si no está presente o tiene cualquier otro valor será blanco)
%%             expire: 5000 // Tiempo en milisegundos (-1 para que se quede fija)
%%         }
%%     }
%% }
%%
%% // Cualquier otro tipo de componente webix (útil para crear el rango de fechas, o mostrar una ventana que requiera una acción del usuario)
%% var popup = {
%%     class: "popup",
%%     value: {
%%         class: "webix_ui", // tipo de popup
%%         position: { // nueva propiedad para delimitar dónde aparecerá el nuevo componente
%%             pos: "center", // ("center" ignora el resto de opciones y centra el componente vertical y horizontalmente, "top" toma como referencia el borde superior del componente cuya id se indica, cualquier otro valor tomará como referencia el borde inferior del componente)
%%             id: "toolbar-path", // ID del componente referencia
%% //            filter: "created_time", // ID del filtro en el caso de datatables
%%             x: 50, // Offset en el eje X (si no se especifica ningún ID, se tomará como referencia la parte izquierda y superior de la ventana)
%%             y: 10  // Offset en el eje Y
%%         },
%%         value: {
%%             view: "window", // Componente tipo "window" con su head y body
%%             id: "window_id",
%%             height: 250,
%%             width: 300,
%%             modal: true, // Si es true, esta ventana oculta el resto de la aplicación y no permite otras interacciones hasta ser cerrada
%%             head: {
%%                 view: "toolbar",
%%                 margin: -4,
%%                 cols: [
%%                     { view: "label", label: "This window can be closed" }, // Header del window
%%                     { view: "icon", icon: "cog", css: "alter",
%%                         click: "webix.message('Cog pressed')" }, // Icono con una acción asociada
%%                     { view:"icon", icon: "times-circle", css: "alter",
%%                         click: "$$('window_id').close();" } // Icono para cerrar la ventana
%%                 ]
%%             },
%%             body: {
%%                 template: "Some text" // Cuerpo del window
%%             }
%%         }
%%     }
%% }
%%
%% // Colocar una ventana encima del filtro de fechas
%%
%% var popup = {
%%     class: "popup",
%%     value: {
%%         class: "webix_ui",
%%         position: {
%%             pos: "top",
%%             x: 0,
%%             y: 10,
%%             id: "domain_tree_resources"
%%         },
%%         value: {
%%             view: "calendar",
%%             weekHeader: true,
%%             date: new Date(2012,3,16),
%%             events: webix.Date.isHoliday,
%%             timepicker: true,
%%             icons: true
%%         }
%%     }
%% }









%% ===================================================================
%% Public functions
%% ===================================================================
