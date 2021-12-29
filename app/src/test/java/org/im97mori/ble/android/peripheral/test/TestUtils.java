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

    public static ActivityScenario<HiltTestAcitivty> createHiltActivity() {
        Intent intent = Intent.makeMainActivity(new ComponentName(ApplicationProvider.getApplicationContext(), HiltTestAcitivty.class))
                .putExtra("androidx.fragment.app.testing.FragmentScenario.EmptyFragmentActivity.THEME_EXTRAS_BUNDLE_KEY", R.style.FragmentScenarioEmptyFragmentActivityTheme);
        return ActivityScenario.launch(intent);
    }

}
