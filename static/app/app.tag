<app>
    <!-- LAYOUT -->
    <navbar></navbar>
    <drawer></drawer>

    <!-- ROUTES -->
    <router>
        <route path="/..">
            <dashboard/>
        </route>
    </router>

    <!-- MODALS -->
    <about/>
    <docs/>


    <style>
        .header {
            color: #ee6e73;
            font-weight: 300;
        }
    </style>


    <script>
        const globalConfig = {
            config: {
                title: 'Gdax Trade App',
                routes: [
                    { label: 'Dashboard', icon: 'euro_symbol', href: '#dashboard' },
                    { label: 'Docs', icon: 'school', href: '#docs', class: 'modal-trigger' },
                    { label: 'About', icon: 'info', href: '#about', class: 'modal-trigger' }
                ],
                restMethods: [
                    'GET',
                    'POST',
                    'DELETE'
                ],
                showDocs: true
            }
        };
        riot.mixin(globalConfig);
    </script>
</app>