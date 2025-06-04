# How to Run Locally

## Quick Start

### Using Docker to run the API

1. Build the API Docker image:
```bash
docker build -t co2-prediction-api .
```

2. Run the container:
```bash
docker run -d -p 8080:8080 --name co2-api co2-prediction-api
```

The API will be available at `http://localhost:8080`