package org.im97mori.ble.android.peripheral.ui.device.setting;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

public class DeviceSettingLauncherContract extends ActivityResultContract<Pair<Long, Integer>, Boolean> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @NonNull Pair<Long, Integer> pair) {
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
