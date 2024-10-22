# Image Detection Haskell

A noble attempt at creating a RESTful web service in haskell

## Requirements

- Haskell (GHC)
- Stack
- PostgreSQL

## Installation

1. **Clone the repository:**
   ```bash
   git clone git@github.com:nathangaar/image-detection-haskell.git
   cd image-detection-haskell
   ```

2. Set up your environment variables for the PostgreSQL connection:

```bash
export DATABASE_NAME="images" 
export DATABASE_USER="image_user" 
export DATABASE_PASSWORD="your_password"
```

3. Install and run:
```bash
stack build
stack run
```

The application will start on port `8080`.
