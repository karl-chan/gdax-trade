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
                                    <a href="#dashboard-form-rest" class="active">REST</a>
                                </b>
                            </li>
                            <li class="tab col s6">
                                <b>
                                    <a href="#dashboard-form-stream" class="">STREAM</a>
                                </b>
                            </li>
                            <div class="indicator green darken-2" style="z-index:1"></div>
                        </ul>
                    </div>
                </div>

                <!-- REST FORM -->
                <dashboard-form-rest></dashboard-form-rest>

                <!-- STREAM FORM -->
                <dashboard-form-stream></dashboard-form-stream>
            </div>
        </div>

        <!-- RIGHT PANE -->
        <dashboard-result></dashboard-result>
    </div>

    <style>
        #dashboard-result {
            margin-top: 3px;
            min-height: calc(100vh - 55px);
        }

        label[for="dashboard-form-rest-endpoint"] {
            width: 300px;
        }

        .tab .active {
            color: #388e3c !important;
        }
    </style>

    <script>
        function DashboardController() {
            const self = this;
            riot.observable(self);
        }

        riot.mixin('DashboardController', {
            dashboardController: new DashboardController()
        });

        const self = this;
        this.on('mount', () => {
            // Initialise tabs (REST / STREAM)
            $('#dashboard-form-tabs').tabs();
        });
    </script>

</dashboard>