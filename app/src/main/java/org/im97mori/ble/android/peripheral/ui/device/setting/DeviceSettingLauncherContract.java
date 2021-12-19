package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

public class DeviceSettingLauncherContract extends ActivityResultContract<Pair<Long, Integer>, Boolean> {

    public static final String KEY_DEVICE_ID = "KEY_DEVICE_ID";

    public static final long UNSAVED_DEVICE_ID = -1;

    public static final String KEY_DEVICE_TYPE = "KEY_DEVICE_TYPE";

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, Pair<Long, Integer> pair) {
        Intent intent = new Intent(context.getApplicationContext(), DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_ID, pair.first);
        intent.putExtra(KEY_DEVICE_TYPE, pair.second);
        return intent;
    }

    @Override
    @NonNull
    public Boolean parseResult(int resultCode, @Nullable Intent intent) {
        return Activity.RESULT_OK == resultCode;
    }
}
