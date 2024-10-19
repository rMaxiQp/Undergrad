/**
 * @author
 */
public class RainGame {
	
	public static void main(String[] args) {
		int x=0, y=0, dx=0, dy=0, score=0, level=1, live=3, count=0;
		int red =0, green =0, blue = 0;
		String text = "";
		long startTime =System.currentTimeMillis();
		
		while (Zen.isRunning()) {
			/************************************Start*********************************/
			boolean start = false;
			while (!start){
				Zen.setColor(0, 200, 200);
				Zen.setFont("Helvetica-64");
				Zen.drawText("Click to Start", 120, Zen.getZenHeight()/2);
				Zen.setColor(200, 0, 200);
				Zen.setFont("Helvetica-32");
				Zen.drawText("'+' to level up", 210, 300);
				if(Zen.getMouseClickX()>0 && Zen.getMouseClickX() < Zen.getZenWidth())
					start = true;
				Zen.flipBuffer();
			}
			/**********************************Setting*********************************/
			Zen.setFont("Helvetica-64");
			long elapsed = System.currentTimeMillis() - startTime;
			{
				Zen.setColor(0, 0, 0);//Set background
				Zen.fillRect(0, 0, Zen.getZenWidth(), Zen.getZenHeight());
				
				Zen.setColor(255,255,255);
				Zen.drawText("Level: " + level,10,50);
				Zen.drawText("Score: " + score,310,50);
				Zen.drawText("Live(s): "+ live, 370, 470);
			}
			
			if (text.length() == 0) {
				x = 0;
				y = Zen.getZenHeight() / 3;
				dx = 0;
				dy = 0;
				if (level%2==0 && level <10)
					dx = (int)(level * 0.3 + 1);
				else if (level <10)
					dy = (int)(level * 0.3 + 1);
				else{
					dx = (int)(level * 0.3 + 1);
					dy = (int)(level * 0.3 + 1);
				}
				startTime = System.currentTimeMillis();
				red = (int)(255-level*Math.random()*5);
				green = (int)(255-level*Math.random()*5);
				blue = (int)(255-level*Math.random()*5);
				text = createText(level);
			}
			
			if(level<10)//Set color
				Zen.setColor(255, 255, 255);
			else if(level<30)
				Zen.setColor(red, green, blue);
			else 
				Zen.setColor((int)(Math.random()*255), (int)(Math.random()*255), (int)(Math.random()*255));
			Zen.drawText(text, x, y);
			//shift value
			x += dx;
			y += dy;

			Zen.flipBuffer();

			/**************************************Input**************************************/
			String user = Zen.getEditText();// Find out what keys the user has been pressing.

			
			for(int i=0; i < user.length(); i++) {
				char c = user.charAt(i);
				if(c =='+')
					level ++;
				if(c == text.charAt(0) && text.length()>0)
					text = text.substring(1,text.length());// all except first character
				if(text.length()==0){
					score += 3000 / elapsed;
					count += score%10;
					if(count >= 10){
						level ++;
						count = count%10;
					}
				}
			}
			
			if (y>Zen.getZenHeight()+50||x>Zen.getZenWidth()+50){//reduce lives and reset text
				live --;
				text = "";
			}		
			
			if (live <= 0){//Game Over
				Zen.setColor(255,0,0);
				Zen.drawText("Game Over", 150, Zen.getZenHeight()/2);
				Zen.flipBuffer();
			}
			
			Zen.setEditText("");
			// Reset the keyboard input to an empty string
			// So next iteration we will only get the most recently pressed keys.
			Zen.sleep(60);// sleep for 60 milliseconds
		}
	}
	
	public static String createText(int level){
		String text = "";
			if(level<5)
				text = "" + (int) (Math.random() * 9);
			else if(level<10)
				text = "" + (int) (Math.random() * 99);
			else
				text = "" + (int) (Math.random() * 999); 
		return text;
	}		
}
