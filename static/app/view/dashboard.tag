<dashboard>
    <div class="row">
        <!-- LEFT PANE -->
        <div id="dashboard-form" class="col s12 l6">
            <div class="container">
                <div class="row">
                    <h2 class="header">Try it out!</h2>
                </div>
                <!-- TABS -->
                <div class="row">
                    <div class="col s12">
                        <ul id="dashboard-form-tabs" class="tabs tabs-fixed-width">
                            <li class="tab col s6">
                                <b>
                                    <a href="#dashboard-rest-form" class="active">REST</a>
                                </b>
                            </li>
                            <li class="tab col s6">
                                <b>
                                    <a href="#dashboard-stream-form" class="">STREAM</a>
                                </b>
                            </li>
                            <div class="indicator green darken-2" style="z-index:1"></div>
                        </ul>
                    </div>
                </div>

                <!-- REST FORM -->
                <form id="dashboard-rest-form">
                    <div class="row">
                        <virtual each={method, i in config.restMethods}>
                            <div class="col s4">
                                <input name="dashboard-rest-form-method" type="radio" id="dashboard-rest-form-method-{method}" class="with-gap" value={method}
                                    checked={i===0}/>
                                <label for="dashboard-rest-form-method-{method}">{method}</label>
                            </div>
                        </virtual>
                    </div>
                    <div class="row input-field">
                        <input placeholder="e.g. /accounts" id="dashboard-rest-form-endpoint" type="text" class="{endpointValid? 'valid': 'invalid'}"
                            oninput={validateEndpoint} onblur={validateEndpoint}>
                        <label for="dashboard-rest-form-endpoint" data-error="It should start with /, e.g. /accounts">Endpoint</label>
                    </div>
                    <div class="row input-field" show={selectedMethod==='POST' }>
                        <textarea placeholder="Stick your JSON payload here!" id="dashboard-rest-form-payload" type="text" class="materialize-textarea validate {payloadValid? 'valid': 'invalid'}"
                            oninput={validatePayload} onblur={formatPayload}></textarea>
                        <label for="dashboard-rest-form-payload" data-error="It must be valid JSON or blank">(Optional) JSON Payload</label>
                    </div>
                    <div class="row">
                        <button class="btn waves-effect waves-light blue darken-4 {disabled:!validateRestSubmit()}" type="submit" name="action">Submit
                            <i class="material-icons right">send</i>
                        </button>
                    </div>
                </form>

                <!-- STREAM FORM -->
                <form id="dashboard-stream-form">
                    Stream
                </form>
            </div>
        </div>

        <!-- RIGHT PANE -->
        <div id="dashboard-result" class="col s12 l6 blue-grey lighten-5">
            <div class="container">
                <div class="row">
                    <h2 class="header">Results</h2>
                </div>
                <!-- ERROR RESULT -->
                <div if={errorResult} id="dashboard-result-error" class="red-text">{errorResult}</div>
                <!-- REST RESULT -->
                <div if={restResult} id="dashboard-result-rest">
                    <div if={restResult.pagination} id="dashboard-result-rest-pagination">
                        <div class="row">
                            <!-- PAGINATION (IF APPLICABLE) -->
                            <ul class="pagination">
                                <li class={restResult.pagination.after? 'waves-effect waves-red': 'disabled'} onclick={paginateAfter}>
                                    <a href="#!">
                                        <i class="material-icons">chevron_left</i>
                                    </a>
                                </li>
                                <li id="dashboard-result-rest-pagination-after-tooltip" if={restResult.pagination.after} onclick={paginateAfter} class="waves-effect waves-red tooltipped"
                                    data-position="left" data-delay="50" data-tooltip={restResult.pagination.after}>
                                    <a href="#!">Older</a>
                                </li>
                                <li class="active">
                                    <a href="#!">Now</a>
                                </li>
                                <li id="dashboard-result-rest-pagination-before-tooltip" if={restResult.pagination.before} onclick={paginateBefore} class="waves-effect waves-teal tooltippe}"
                                    data-position="right" data-delay="50" data-tooltip={restResult.pagination.before}>
                                    <a href="#!">Newer</a>
                                </li>
                                <li class={restResult.pagination.before? 'waves-effect waves-teal': 'disabled'} onclick={paginateBefore}>
                                    <a href="#!">
                                        <i class="material-icons">chevron_right</i>
                                    </a>
                                </li>
                            </ul>
                        </div>
                    </div>
                    <div id="dashboard-result-rest-json"></div>
                </div>
                <!-- STREAM RESULT -->
            </div>
        </div>
    </div>

    <style>
        #dashboard-result {
            margin-top: 3px;
            min-height: calc(100vh - 55px);
        }

        label[for="dashboard-rest-form-endpoint"] {
            width: 300px;
        }

        .tab .active {
            color: #388e3c !important;
        }
    </style>

    <script>
        const self = this;
        this.on('mount', () => {
            $('#dashboard-form-tabs').tabs();

            // Need to manually register select due to materialise css library manipulating DOM
            $('input[name="dashboard-rest-form-method"]').change((e) => {
                this.selectedMethod = event.target.value;
                this.update();
            });

            // Bind jquery to form submit
            $('#dashboard-rest-form').submit((e) => {
                if (!this.validateRestSubmit()) {
                    return false;
                }
                const method = this.selectedMethod;
                const endpoint = $('#dashboard-rest-form-endpoint').val().trim();
                const payload = $('#dashboard-rest-form-payload').val().trim();
                this.fireRestRequest(method, endpoint, payload);
                e.preventDefault();
            });
        });

        this.selectedMethod = 'GET';
        this.endpointValid = false;
        this.payloadValid = true;
        this.errorResult = false;
        this.restResult = undefined;
        this.paginationResult = undefined;
        this.endpointHistory = {};

        validateEndpoint(e) {
            this.endpointValid = e.target.value.startsWith('/');
            e.preventDefault();
        };

        validatePayload(e) {
            const userInput = e.target.value.trim();
            if (!userInput) {
                this.payloadValid = true;
                return;
            }
            try {
                JSON.parse(userInput);
                this.payloadValid = true;
            } catch (ex) {
                this.payloadValid = false;
            }
        }

        formatPayload(e) {
            const userInput = e.target.value.trim();
            try {
                const payload = JSON.parse(userInput);
                const formatted = JSON.stringify(payload, null, '\t');
                $('#dashboard-rest-form-payload').val(formatted);
                $('#dashboard-rest-form-payload').trigger('autoresize');
            } catch (ex) {
            }
        }

        validateRestSubmit() {
            return this.endpointValid && this.payloadValid;
        }

        showErrorResult(method, endpoint, error) {
            this.errorResult = error;
            this.restResult = undefined;
            this.update();
        }

        showRestResult(method, endpoint, body, before, after) {
            this.errorResult = undefined;
            const pagination = before && after ? {
                before: before,
                after: after
            } : undefined;
            this.restResult = {
                method: method,
                endpoint: endpoint,
                pagination: pagination,
                body: body
            }
            this.update();
            $('#dashboard-result-rest-json').jJsonViewer(body);
            $('#dashboard-result-rest-pagination-after-tooltip').tooltip();
            $('#dashboard-result-rest-pagination-before-tooltip').tooltip();
        }

        paginateAfter() {
            const method = this.restResult.method;
            const endpoint = this.restResult.endpoint;
            const after = this.restResult.pagination.after;
            this.fireRestRequest(method, endpoint, undefined, undefined, after);
        }

        paginateBefore() {
            const method = this.restResult.method;
            const endpoint = this.restResult.endpoint;
            const before = this.restResult.pagination.before;
            this.fireRestRequest(method, endpoint, undefined, before, undefined);
        }

        fireRestRequest(method, endpoint, payload, before, after) {
            const requestBody = {
                method: method,
                endpoint: endpoint
            }
            if (payload) {
                // trim whitespace in JSON due to display formatting
                requestBody['payload'] = JSON.stringify(JSON.parse(payload));
            }
            if (before) {
                requestBody.endpoint = URI(requestBody.endpoint).addSearch('before', before).toString();
            }
            if (after) {
                requestBody.endpoint = URI(requestBody.endpoint).addSearch('after', after).toString();
            }

            $.ajax({
                type: 'POST',
                url: '/api/rest',
                data: requestBody,
                success: (response) => {
                    if ('err' in response) {
                        self.showErrorResult(method, endpoint, response.err);
                    } else {
                        self.addToEndpointHistory(endpoint);
                        self.showRestResult(method, endpoint, response.body, response.before, response.after);
                    }
                },
                error: (xhr, ajaxOptions, thrownError) => {
                    const error = `${xhr.status} ${xhr.responseText}`;
                    self.showErrorResult(method, endpoint, error);
                }
            });
        }

        addToEndpointHistory(endpoint) {
            this.endpointHistory[endpoint] = null;
            $('#dashboard-rest-form-endpoint').autocomplete({
                data: this.endpointHistory,
                minLength: 0
            });
        }
    </script>

</dashboard>