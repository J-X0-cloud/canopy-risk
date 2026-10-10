# syntax=docker/dockerfile:1

FROM eclipse-temurin:21-jdk AS build
WORKDIR /src
RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates gzip \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fsSL https://github.com/coursier/coursier/releases/download/v2.1.24/cs-x86_64-pc-linux.gz \
    | gzip -d > /usr/local/bin/cs \
 && chmod +x /usr/local/bin/cs \
 && cs install --install-dir /usr/local/bin sbt
COPY project/build.properties project/plugins.sbt project/
COPY build.sbt .
RUN sbt -batch update
COPY src src
RUN sbt -batch assembly

FROM eclipse-temurin:21-jre
RUN useradd --system --uid 10001 canopy
WORKDIR /app
COPY --from=build /src/target/scala-3.3.7/canopy-risk.jar /app/canopy-risk.jar
USER canopy
ENV PORT=8080
EXPOSE 8080
# The serve command reads $PORT when --port is not given.
ENTRYPOINT ["java", "-XX:MaxRAMPercentage=75", "-jar", "/app/canopy-risk.jar"]
CMD ["serve"]
