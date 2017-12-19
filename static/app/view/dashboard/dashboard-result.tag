<dashboard-result>
    <div id="dashboard-result" class="col s12 l6 blue-grey lighten-5">
        <div class="container">
            <div class="row">
                <h2 class="header">Results</h2>
            </div>
            <!-- ERROR RESULT -->
            <div if={errorResult}>
                <blockquote>
                    <b>{errorResult.title}</b>
                </blockquote>
                <div class="red-text">{errorResult.error}</div>
            </div>
            <!-- REST RESULT -->
            <div if={restResult}>
                <blockquote>
                    Response from
                    <b>{restResult.method} {restResult.endpoint}</b>
                </blockquote>
                <div if={restResult.pagination}>
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
            <div if={streamResult}>
                <blockquote>
                    Response from
                    <br> Products:
                    <b>{streamResult.products.join(', ')}</b>
                    <br> Channels:
                    <b>{streamResult.channels.join(', ')}</b>
                </blockquote>
                <div class="fixed-action-btn toolbar">
                    <a class="btn-floating btn-large {streamResult.write? 'red': 'green'}" onclick={streamResult.write? pauseStream: playStream}>
                        <i class="large material-icons">{streamResult.write? 'pause' : 'play_arrow'}</i>
                    </a>
                    <ul>
                        <li class="waves-effect waves-light" onclick={playStream}>
                            <a href="#!">
                                <i class="material-icons">play_arrow</i>
                            </a>
                        </li>
                        <li class="waves-effect waves-light" onclick={pauseStream}>
                            <a href="#!">
                                <i class="material-icons">pause</i>
                            </a>
                        </li>
                        <li class="waves-effect waves-light" onclick={clearStream}>
                            <a href="#!">
                                <i class="material-icons">clear</i>
                            </a>
                        </li>
                    </ul>
                </div>
                <div id="dashboard-result-stream-json"></div>
            </div>
        </div>
    </div>

    <script>
        this.mixin('DashboardController'); // provides controller
        const self = this;

        this.dashboardController.on('restResponse', (error, method, endpoint, body, before, after) => {
            if (error) {
                const title = `Error from ${method} ${endpoint}`;
                this.showErrorResult(title, error);
            } else {
                this.showRestResult(method, endpoint, body, before, after);
            }
        });

        this.dashboardController.on('streamResponse', (error, products, channels, socket) => {
            if (error) {
                const title = `Error from\nProducts: ${products.join(', ')}\nChannels: ${channels.join(', ')}`
                this.showErrorResult(title, error);
            } else {
                this.showStreamResult(products, channels, socket);
            }
        });

        this.errorResult = undefined;
        this.restResult = undefined;
        this.streamResult = undefined;

        showErrorResult(title, error) {
            this.errorResult = {
                title: title,
                error: error
            };
            this.restResult = undefined;
            this.streamResult = undefined;
            this.update();
        }

        showRestResult(method, endpoint, body, before, after) {
            this.errorResult = undefined;
            this.restResult = {
                method: method,
                endpoint: endpoint,
                pagination: before && after ? {
                    before: before,
                    after: after
                } : undefined,
                body: body
            }
            this.streamResult = undefined;
            this.update();
            $('#dashboard-result-rest-json').jJsonViewer(body);
            $('#dashboard-result-rest-pagination-after-tooltip').tooltip();
            $('#dashboard-result-rest-pagination-before-tooltip').tooltip();
        }

        showStreamResult(products, channels, socket) {
            this.errorResult = undefined;
            this.restResult = undefined;
            this.streamResult = {
                products: products,
                channels: channels,
                socket: socket,
                write: true
            }
            $('#dashboard-result-stream.json').empty();
            socket.onmessage = event => {
                if (this.streamResult.write) {
                    $('#dashboard-result-stream-json').append(event.data);
                }
            }
            this.update();
        }

        paginateAfter() {
            const method = this.restResult.method;
            const endpoint = this.restResult.endpoint;
            const after = this.restResult.pagination.after;
            this.dashboardController.trigger('restRequest', method, endpoint, undefined, undefined, after);
        }

        paginateBefore() {
            const method = this.restResult.method;
            const endpoint = this.restResult.endpoint;
            const before = this.restResult.pagination.before;
            this.dashboardController.trigger('restRequest', method, endpoint, undefined, before, undefined);
        }

        playStream() {
            if (this.streamResult) {
                this.streamResult.write = true;
            }
        }

        pauseStream() {
            if (this.streamResult) {
                this.streamResult.write = false;
            }
        }

        clearStream() {
            $('#dashboard-result-stream-json').empty();
        }
    </script>
</dashboard-result>