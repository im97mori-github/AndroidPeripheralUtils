package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_PROPERTIES_TYPE;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

public class ClientCharacteristicConfigurationLauncherContract extends ActivityResultContract<Pair<byte[], Integer>, byte[]> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @NonNull Pair<byte[], Integer> input) {
        Intent intent = new Intent(context.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class);
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), input.first);
        intent.putExtra(KEY_PROPERTIES_TYPE, input.second);
        return intent;
    }

    @Override
    public byte[] parseResult(int resultCode, @Nullable Intent intent) {
        byte[] data;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            data = intent.getByteArrayExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString());
        } else {
            data = null;
        }
        return data;
    }
}
