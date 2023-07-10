set -e

echo "==== build ===="
# docker build --platform linux/x86_64 --progress=plain --rm -t nobanashi-backend .
docker build --progress=plain --rm -t nobanashi-backend .

echo "==== run ===="
export PORT=8080
docker run -it --env NOBANASHI_PASS=pass --env PORT=$PORT --expose $PORT nobanashi-backend
