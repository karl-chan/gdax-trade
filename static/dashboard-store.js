function DashboardStore() {
	riot.observable(this); // Riot provides our event emitter.

	const self = this;

	self.dashboard = {
		forms: {
			rest: {
				method: undefined,
				endpoint: undefined
			},
			stream: {
				product: undefined,
				channels: []
			}
		},
		display: {
			error: undefined,
			json: {}
		}
	};

	self.on("dashboard_init", () => {
		self.trigger("dashboard_update", self.dashboard);
	});
}
