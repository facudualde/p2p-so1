FROM erlang:latest

ARG user

# Create non-root user with home directory
RUN useradd --create-home $user

# Create project directory using shell so ARG gets expanded properly
RUN mkdir -p /home/$user/p2p-so1

# Copy project files into the desired path
COPY . /home/$user/p2p-so1

# Set permissions so the user can access the files
RUN chown -R $user:$user /home/$user

# Set working directory to the project path
WORKDIR /home/$user/p2p-so1
