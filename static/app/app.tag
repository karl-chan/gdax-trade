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
    <live/>
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
                    { label: 'Dashboard', icon: 'dashboard', href: '#dashboard' },
                    { label: 'Live', icon: 'graphic_eq', href: 'https://www.gdax.com/trade', target: '_blank' },
                    { label: 'Docs', icon: 'school', href: '#docs', class: 'modal-trigger' },
                    { label: 'About', icon: 'info', href: '#about', class: 'modal-trigger' }
                ],
                restMethods: ['GET', 'POST', 'DELETE'],
                products: [
                    'BTC-USD', 'BTC-EUR', 'BTC-GBP',
                    'ETH-USD', 'ETH-BTC', 'ETH-EUR',
                    'LTC-USD', 'LTC-BTC', 'LTC-EUR'
                ],
                channels: [
                    'heartbeat', 'ticker', 'level2', 'user', 'matches', 'full'
                ],
                showDocs: true
            }
        };
        riot.mixin(globalConfig);
    </script>
</app>