from googleapiclient.discovery import build
from youtube_transcript_api import YouTubeTranscriptApi
import csv
from datetime import datetime, timezone

# Replace with your YouTube Data API key
API_KEY = Yourapikey
def get_channel_videos(channel_id):
    """
    Retrieves a list of all videos published by a given channel ID, including recent content,
    and saves the data to a CSV file.

    Args:
        channel_id: The ID of the YouTube channel.
    """
    # Initialize the YouTube API service
    youtube = build("youtube", "v3", developerKey=API_KEY)
    
    videos = []
    next_page_token = None
    
    # Get current date in ISO 8601 format
    current_date = datetime.now(timezone.utc).isoformat()
    #current_date ="2024-01-01T00:00:00Z"
    # Set a date far in the past to ensure we get all videos
    start_date = "2025-02-18T00:00:00Z"

    while True:
        request = youtube.search().list(
            part="snippet",
            channelId=channel_id,
            maxResults=50,  # Maximum allowed by API
            pageToken=next_page_token,
            type="video",
            order="date",  # Sort by publication date
            publishedAfter=start_date,
            publishedBefore=current_date
        )
        
        try:
            response = request.execute()
        except Exception as e:
            print(f"Error executing request: {e}")
            break

        for item in response.get("items", []):
            try:
                video_id = item["id"]["videoId"]
                video_title = item["snippet"]["title"]
                published_at_str = item["snippet"]["publishedAt"]
                video_link = f"https://www.youtube.com/watch?v={video_id}"
                
                # Try to get Spanish transcript, fall back to any available language
                try:
                    transcript = YouTubeTranscriptApi.get_transcript(video_id, languages=['es'])
                except:
                    try:
                        transcript = YouTubeTranscriptApi.get_transcript(video_id)
                    except:
                        transcript = "Transcript not available"

                videos.append({
                    "id": video_id,
                    "title": video_title,
                    "published_at": published_at_str,
                    "link": video_link,
                    "transcript": transcript
                })
                
                print(f"Retrieved video: {video_title} ({published_at_str})")
                
            except Exception as e:
                print(f"Error processing video: {e}")
                continue

        next_page_token = response.get("nextPageToken")
        
        if not next_page_token:
            break

    # Save data to CSV
    if videos:
        try:
            with open("youtube_videos3.csv", "w", newline="", encoding="utf-8") as csvfile:
                fieldnames = ["id", "title", "published_at", "link", "transcript"]
                writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
                writer.writeheader()
                writer.writerows(videos)
            print(f"Successfully saved {len(videos)} videos to youtube_videos.csv")
        except Exception as e:
            print(f"Error saving to CSV: {e}")
    else:
        print("No videos found to save")

if __name__ == "__main__":
    channel_id = "UCz489cQmrgH57sShDiatwfw"  # Channel ID for elpelucamilei
    get_channel_videos(channel_id)
