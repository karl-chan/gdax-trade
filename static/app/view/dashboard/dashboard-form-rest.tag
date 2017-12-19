<dashboard-form-rest>
    <form id="dashboard-form-rest">
        <div class="row">
            <virtual each={method, i in config.restMethods}>
                <div class="col s4">
                    <input name="dashboard-form-rest-method" type="radio" id="dashboard-form-rest-method-{method}" class="with-gap" value={method}
                        checked={i===0}/>
                    <label for="dashboard-form-rest-method-{method}">{method}</label>
                </div>
            </virtual>
        </div>
        <div class="row input-field">
            <input placeholder="e.g. /accounts" id="dashboard-form-rest-endpoint" type="text" class="{endpointValid? 'valid': 'invalid'}"
                oninput={validateEndpoint} onblur={validateEndpoint}>
            <label for="dashboard-form-rest-endpoint" data-error="It should start with /, e.g. /accounts">Endpoint</label>
        </div>
        <div show={selectedMethod==='POST' }>
            <div class="row input-field">
                <textarea placeholder="Stick your JSON payload here!" id="dashboard-form-rest-payload" type="text" class="materialize-textarea validate {payloadValid? 'valid': 'invalid'}"
                    oninput={validatePayload} onblur={formatPayload}></textarea>
                <label for="dashboard-form-rest-payload" data-error="It must be valid JSON or blank">(Optional) JSON Payload</label>
            </div>
        </div>
        <div class="row">
            <button class="btn waves-effect waves-light blue darken-4 {disabled:!validateSubmit()}" type="submit" name="action">Submit
                <i class="material-icons right">send</i>
            </button>
        </div>
    </form>

    <script>
        this.mixin('DashboardController'); // provides controller
        const self = this;

        this.dashboardController.on('restRequest', (method, endpoint, payload, before, after) => {
            this.fireRestRequest(method, endpoint, payload, before, after);
        });

        this.selectedMethod = 'GET';
        this.endpointValid = false;
        this.payloadValid = true;
        this.payloadSwitchJSON = false;
        this.endpointHistory = {};

        this.on('mount', () => {
            // Bind jquery to form submit
            $('#dashboard-form-rest').submit((e) => {
                if (!this.validateSubmit()) {
                    return false;
                }
                const method = this.selectedMethod;
                const endpoint = $('#dashboard-form-rest-endpoint').val().trim();
                const payload = $('#dashboard-form-rest-payload').val().trim();
                this.fireRestRequest(method, endpoint, payload);
                e.preventDefault();
            });

            // Need to manually register select due to materialise css library manipulating DOM
            $('input[name="dashboard-form-rest-method"]').change((e) => {
                this.selectedMethod = event.target.value;
                this.update();
            });
        });

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

        payloadSwitchChange(e) {
            this.payloadSwitchJSON = e.target.checked;
        }

        formatPayload(e) {
            const userInput = e.target.value.trim();
            try {
                const payload = JSON.parse(userInput);
                const formatted = JSON.stringify(payload, null, '\t');
                $('#dashboard-form-rest-payload').val(formatted);
                $('#dashboard-form-rest-payload').trigger('autoresize');
            } catch (ex) {
            }
        }

        validateSubmit() {
            return this.endpointValid && this.payloadValid;
        }

        addToEndpointHistory(endpoint) {
            this.endpointHistory[endpoint] = null;
            $('#dashboard-form-rest-endpoint').autocomplete({
                data: this.endpointHistory,
                minLength: 0
            });
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
                        self.handleRestResponse(response.err, method, endpoint);
                    } else {
                        self.addToEndpointHistory(endpoint);
                        self.handleRestResponse(undefined, method, endpoint, response.body, response.before, response.after);
                    }
                },
                error: (xhr, ajaxOptions, thrownError) => {
                    const error = `${xhr.status} ${xhr.responseText}`;
                    self.handleRestResponse(error, method, endpoint);
                }
            });
        }

        handleRestResponse(error, method, endpoint, body, before, after) {
            this.dashboardController.trigger('restResponse', error, method, endpoint, body, before, after);
        }
    </script>

</dashboard-form-rest>