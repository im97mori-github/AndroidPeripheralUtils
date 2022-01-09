package org.im97mori.ble.android.peripheral.test;

import android.content.ComponentName;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.Drawable;

import androidx.annotation.NonNull;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;

import org.im97mori.ble.android.peripheral.R;

public final class TestUtils {

    @NonNull
    public static Bitmap getBitmap(Drawable drawable) {
        Bitmap bitmap = Bitmap.createBitmap(drawable.getIntrinsicWidth(),
                drawable.getIntrinsicHeight(), Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight());
        drawable.draw(canvas);
        return bitmap;
    }

    public static ActivityScenario<HiltTestActivity> createHiltActivity() {
        Intent intent = Intent.makeMainActivity(new ComponentName(ApplicationProvider.getApplicationContext(), HiltTestActivity.class))
                .putExtra("androidx.fragment.app.testing.FragmentScenario.EmptyFragmentActivity.THEME_EXTRAS_BUNDLE_KEY", R.style.FragmentScenarioEmptyFragmentActivityTheme);
        return ActivityScenario.launch(intent);
    }

    public static String getCurrentMethodName() {
        // default stacktrace index is next of this method
        int index = 1;

        // find this method index
        StackTraceElement[] stackTraceElementArray = Thread.currentThread().getStackTrace();
        for (int i = 0; i < stackTraceElementArray.length; i++) {
            StackTraceElement stackTraceElement = stackTraceElementArray[i];
            if (TestUtils.class.getName().equals(stackTraceElement.getClassName())
                    && "getCurrentMethodName".equals(stackTraceElement.getMethodName())) {
                index += i;
                break;
            }
        }

        StackTraceElement stackTraceElement = stackTraceElementArray[index];
        return stackTraceElement.getMethodName();
    }
}
